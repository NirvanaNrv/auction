package exo.auction

import java.time.LocalDateTime
import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.Uri.Path
import akka.http.scaladsl.model.{HttpEntity, StatusCodes, Uri}
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

class PropertyAuctionSpec extends AuctionSpec with PropertyChecks {
	//implicit override val generatorDrivenConfig = PropertyCheckConfig(minSuccessful = 2, maxDiscarded = 10)

	"The auction service" should {
		"allow to create the auction" in {
			val generator = for {
				initialPrice <- Gen.choose(0.0, 100)
				firstBetDelta <- Gen.choose(0.0, 100)
				secondBetDelta <- Gen.choose(0.0, 100)
			} yield (initialPrice, firstBetDelta, secondBetDelta)
			var i = 0

			forAll(generator) {case (initialPriceDelta: Double, firstBetDelta: Double, secondBetDelta: Double) =>
				val initialPrice = cent + initialPriceDelta
				val item = s"item #$i"
				val newAuction = newAuctionObject(initialPrice).copy(item = item)
				i = i + 1
				val policy = newAuction.parameters.policy
				val firstBet = policy.minimumBid(initialPrice) + firstBetDelta
				val secondBet = policy.minimumBid(firstBet) + secondBetDelta

				Post("/auctions", HttpEntity(`application/json`, auctionPayload(newAuction))) ~> route ~> check {
					status shouldEqual StatusCodes.Created
				}
				setTime = Some(LocalDateTime.parse("2018-01-01T00:00:00"))
				//TODO avoid forth and back in time (forth only is fine)
				Post(Uri(path = Path / "auctions" / item / "bidders"), HttpEntity(`application/json`, bidder.toString)) ~> route ~> check {
					responseAs[String] shouldEqual ""
					status shouldBe StatusCodes.OK
				}
				setTime = Some(LocalDateTime.parse("2019-01-01T00:00:00"))
				Post(Uri(path = Path / "auctions" / item / "bids"), HttpEntity(`application/json`, newBid(firstBet))) ~> route ~> check {
					responseAs[String] shouldEqual "" //Allows a clear report for the failing tests
					status shouldBe StatusCodes.Created
				}
				Post(Uri(path = Path / "auctions" / item / "bids"), HttpEntity(`application/json`, newBid(secondBet))) ~> route ~> check {
					responseAs[String] shouldEqual ""
					status shouldBe StatusCodes.Created
				}

				val minPrice = policy.minimumBid(secondBet)
				forAll(Gen.choose(0.0, 100)) {lowPriceDelta: Double =>
					val lowPrice = minPrice - cent - lowPriceDelta
					Post(Uri(path = Path / "auctions" / item / "bids"), HttpEntity(`application/json`, newBid(lowPrice))) ~> route ~> check {
						status shouldBe StatusCodes.Conflict
					}
				}

				setTime = Some(LocalDateTime.parse("2019-01-01T01:00:00"))
				Get(Uri(path = Path / "auctions" / item / "winner")) ~> route ~> check {
					status shouldEqual StatusCodes.OK
					if (status == StatusCodes.OK) {
						import JsonSupport._
						responseAs[Winner].price shouldBe secondBet //TODO seems to work, but need to check what doubles do about precision
					}
				}
			}
		}
	}
}
