package exo.auction

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import akka.stream.ActorMaterializer
import scala.concurrent.Future
import scala.io.StdIn
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.util.Timeout
import spray.json._

import scala.concurrent.duration._
import scala.util.Success

object WebServer {
	val port = 5000
	val currency = "€"
	val cent = 0.01

	def main(args: Array[String]) {
		implicit val system = ActorSystem("auction-web")
		implicit val materializer = ActorMaterializer()
		implicit val executionContext = system.dispatcher

		import akka.actor.typed.scaladsl.AskPattern._
		import AuctionHouse.{system => auctionHouse, _}
		implicit val scheduler = auctionHouse.scheduler

		object JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
			implicit val auctioneerFormat = jsonFormat1(Auctioneer)
			implicit val policyFormat = jsonFormat1(Policy.apply)
			implicit object LocalDateTimeFormat extends JsonFormat[LocalDateTime] {
				def write(dateTime: LocalDateTime) = JsString(dateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
				def read(value: JsValue) = value match {
					case JsString(dateTime) => LocalDateTime.parse(dateTime, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
					case _ => deserializationError("LocalDateTime expected.")
				}
			}
			implicit val auctionParametersFormat = jsonFormat5(AuctionParameters)
			implicit val initialAuctionFormat = jsonFormat2(InitialAuction)
			implicit val bidderFormat = jsonFormat1(Bidder)
			implicit val bidFormat = jsonFormat2(Bid)
		}
		JsonSupport

		val route =
			pathPrefix("auctions") {
				import AuctioneerCommand._
				import JsonSupport._
				post {
					implicit val timeout: Timeout = 1.second
					entity(as[InitialAuction]) {initial =>
						val create: Future[Create.Response] = auctionHouse ? (ref => AuctioneerCommand(Create(ref, initial)))
						onComplete(create) {
							case Success(Create.Response(true)) => complete(StatusCodes.Created)
							case Success(Create.Response(false)) => complete(StatusCodes.Conflict)
							case _ => complete(StatusCodes.InternalServerError)
						}
					}
				} ~
				put {
					implicit val timeout: Timeout = 1.second
					path(Segment) {item =>
						entity(as[AuctionParameters]) {parameters =>
							val update: Future[Update.Response] = auctionHouse ? (ref => AuctioneerCommand(Update(ref, InitialAuction(item, parameters))))
							onComplete(update) {
								case Success(Update.Response(true)) => complete(StatusCodes.OK)
								case Success(Update.Response(false)) => complete(StatusCodes.NotFound)
								case _ => complete(StatusCodes.InternalServerError)
							}
						}
					}
				} ~
				get {
					implicit val timeout: Timeout = 1.second
					path(Segment) {item =>
						val get: Future[Interrogate.Response] = auctionHouse ? (ref => AuctioneerCommand(Interrogate(ref, item)))
						onComplete(get) {
							case Success(Interrogate.Response(history)) => complete(history)
							case _ => complete(StatusCodes.InternalServerError)
						}
					}
				}
			}

		val bindingFuture = Http().bindAndHandle(route, "localhost", port)

		println(s"Auction server online at http://localhost:$port/\nPress <Return> to stop...")
		StdIn.readLine
		bindingFuture
			.flatMap(_.unbind)
			.onComplete(_ => system.terminate)
	}
}
