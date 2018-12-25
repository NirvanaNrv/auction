package exo.auction

import java.time.LocalDateTime
import akka.http.scaladsl.model.{HttpEntity, MediaTypes, StatusCodes, Uri}
import MediaTypes._
import akka.http.scaladsl.model.Uri.Path

class NotRegisteredAuctionSpec extends AuctionSpec {
	setTime = Some(LocalDateTime.parse("2018-01-01T00:00:00"))

	"The auction service" should {
		"return 'not found' when querying the not yet existing auction state" in {
			Get((Path / "auctions" / item).toString) ~> route ~> check {
				status shouldEqual StatusCodes.NotFound
			}
		}

		"allow to create the auction" in {
			Post("/auctions", HttpEntity(`application/json`, auction.toString)) ~> route ~> check {
				status shouldEqual StatusCodes.Created
			}
		}

		"then return that empty state when querying that new auction" in {
			Get(Uri(path = Path / "auctions" / item)) ~> route ~> check {
				status shouldEqual StatusCodes.OK
				responseAs[String] shouldEqual "[]"
			}
		}

		"not allow bidding as long until that auction is open" in {
			Post(Uri(path = Path / "auctions" / item / "bids"), HttpEntity(`application/json`, bid.toString)) ~> route ~> check {
				status shouldBe StatusCodes.Conflict
			}
		}

		"not allow bidding as once that auction is open if the bidder did not register in time" in {
			setTime = Some(LocalDateTime.parse("2019-01-01T00:00:00"))
			Post(Uri(path = Path / "auctions" / item / "bids"), HttpEntity(`application/json`, bid.toString)) ~> route ~> check {
				status shouldBe StatusCodes.Conflict
			}
		}
	}
}
