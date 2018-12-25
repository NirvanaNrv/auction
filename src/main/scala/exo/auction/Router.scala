package exo.auction

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import akka.http.scaladsl.model.StatusCodes
import akka.http.scaladsl.server.Directives._
import scala.concurrent.Future
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.Route
import akka.util.Timeout
import spray.json._
import scala.concurrent.duration._
import scala.util.Success

class Router {
	import akka.actor.typed.scaladsl.AskPattern._
	import AuctionHouse._

	protected val auctionHouse = AuctionHouse.createSystem
	implicit val scheduler = auctionHouse.scheduler

	def terminate = {
		auctionHouse.terminate
	}

	val route: Route = {
		import JsonSupport._
		pathPrefix("auctions") {
			post {
				pathEnd {
					import AuctioneerCommand._
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
				pathPrefix(Segment) {item =>
					path("bids") {
						entity(as[Bid]) {bid =>
							import BidderCommand._
							implicit val timeout: Timeout = 1.second
							val bidding: Future[Bid.Response] = auctionHouse ? (ref => BidderCommand(bid.bidder, Bid(ref, item, bid.price)))
							onComplete(bidding) {
								case Success(Bid.Put) => complete(StatusCodes.Created, "")
								case Success(Bid.Denied(e)) => complete(StatusCodes.Conflict, e.toString)
								case Success(Bid.NotFound) => complete(StatusCodes.NotFound)
								case _ => complete(StatusCodes.InternalServerError)
							}
						}
					} ~
					path("bidders") {
						entity(as[Bidder]) {bidder =>
							import BidderCommand._
							implicit val timeout: Timeout = 1.second
							val bidding: Future[Join.Response] = auctionHouse ? (ref => BidderCommand(bidder, Join(ref, item)))
							onComplete(bidding) {
								case Success(Join.Joined) => complete(StatusCodes.OK, "")
								case Success(Join.Denied(e)) => complete(StatusCodes.Conflict, e.toString)
								case Success(Join.NotFound) => complete(StatusCodes.NotFound)
								case _ => complete(StatusCodes.InternalServerError)
							}
						}
					}
				}
			} ~
			put {
				path(Segment) {item =>
					pathEnd {
						import AuctioneerCommand._
						entity(as[AuctionParameters]) { parameters =>
							implicit val timeout: Timeout = 1.second
							val update: Future[Update.Response] = auctionHouse ? (ref => AuctioneerCommand(Update(ref, InitialAuction(item, parameters))))
							onComplete(update) {
								case Success(Update.Response(true)) => complete(StatusCodes.OK)
								case Success(Update.Response(false)) => complete(StatusCodes.NotFound)
								case _ => complete(StatusCodes.InternalServerError)
							}
						}
					}
				}
			} ~
			get {
				import AuctioneerCommand._
				path(Segment) {item =>
					implicit val timeout: Timeout = 1.second
					val get: Future[Interrogate.Response] = auctionHouse ? (ref => AuctioneerCommand(Interrogate(ref, item)))
					onComplete(get) {
						case Success(Interrogate.Found(history)) => complete(history)
						case Success(Interrogate.NotFound) => complete(StatusCodes.NotFound)
						case _ => complete(StatusCodes.InternalServerError)
					}
				}
			}
		} ~
		pathPrefix("bidders" / Segment) {bidderName =>
			parameters('state.*, 'mine.as[Boolean].?) {(stateNames, mineOption) =>
				val states = if (stateNames.nonEmpty) stateNames.map(Auction.State.fromName).toSet else Auction.State.values
				val mine = mineOption.getOrElse(true)
				val bidder = Bidder(bidderName)
				import BidderCommand._
				implicit val timeout: Timeout = 1.second
				val get: Future[Interrogate.Response] = auctionHouse ? (ref => BidderCommand(bidder, Interrogate(ref, states, mine)))
				onComplete(get) {
					case Success(Interrogate.Response(auctions)) => complete(auctions)
					case _ => complete(StatusCodes.InternalServerError)
				}
			}
		}
	} ~
	path("setTime" / Segment) {value => //Test only
		setTime = Some(value).filter(_.nonEmpty).map(LocalDateTime.parse)
		complete(StatusCodes.OK)
	}
}
