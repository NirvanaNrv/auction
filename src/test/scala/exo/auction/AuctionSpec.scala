package exo.auction

import java.time.LocalDateTime
import org.scalatest.{Matchers, WordSpec}
import akka.http.scaladsl.testkit.ScalatestRouteTest
import akka.http.scaladsl.unmarshalling.Unmarshal
import scala.concurrent.Await
import scala.concurrent.duration._

abstract class AuctionSpec extends WordSpec with Matchers with ScalatestRouteTest {
	import spray.json._
	import JsonSupport._

	val item = "Lot #9: Caesar bust"
	def auctionPayload(auction: InitialAuction) = auction.toJson.toString
	def bidPayload(bid: Bid) = bid.toJson.toString
	val auctionObject = InitialAuction(item, AuctionParameters(Auctioneer("nicolas"), 42, Policy.Constant(10), LocalDateTime.parse("2019-01-01T00:00:00"), LocalDateTime.parse("2019-01-01T01:00:00")))
	val auction = auctionPayload(auctionObject)
	//val auction = s"""{"item":"$item","parameters":{"auctioneer":{"name":"nicolas"},"price":42,"policy":{"type":"Constant","value":10},"start":"2019-01-01T00:00:00","end":"2019-01-01T01:00:00"}}"""
	val bidObject = Bid(Bidder("nicolas"), 73)
	val bid = bidPayload(bidObject)
	def newBid(price: Double) = bidPayload(bidObject.copy(price = price))
	val bidderObject = Bidder("nicolas")
	val bidder = bidderObject.toJson.toString
	def marshalAuctions(auction: String) = Unmarshal(auction).to[Seq[Auction]]
	def newAuctionObject(price: Double) = {
		val newParameters = auctionObject.parameters.copy(price = price)
		auctionObject.copy(parameters = newParameters)
	}
	val router = new Router
	val route = router.route

	override protected def beforeAll =  {
		setTime = Some(LocalDateTime.parse("2018-01-01T00:00:00"))
	}


	override protected def afterAll =  {
		Await.ready(router.terminate, 5.seconds)
	}
}
