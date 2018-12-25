package exo.auction

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._

object JsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
	implicit val auctioneerFormat = jsonFormat1(Auctioneer)
	implicit val constantPolicyFormat = jsonFormat1(Policy.Constant)
	implicit val policyFormat = new RootJsonFormat[Policy] {
		def write(obj: Policy): JsValue =
			JsObject((obj match {
				case constant: Policy.Constant => constant.toJson
				//case slice: Policy.Slice => slice.toJson
			}).asJsObject.fields + ("type" -> JsString(obj.productPrefix)))

		def read(json: JsValue): Policy =
			json.asJsObject.getFields("type") match {
				case Seq(JsString("Constant")) => json.convertTo[Policy.Constant]
				//case Seq(JsString("Slice")) => json.convertTo[Slice]
			}
	}
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
	implicit val auctionFormat = jsonFormat3(Auction.apply)
	implicit val winnerFormat = jsonFormat2(Winner)
}
