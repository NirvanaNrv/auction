package exo.auction

import java.time.LocalDateTime

import akka.http.scaladsl.model.MediaTypes._
import akka.http.scaladsl.model.Uri.{Path, Query}
import akka.http.scaladsl.model.{HttpEntity, StatusCodes, Uri}
import scala.concurrent.Await
import scala.concurrent.duration._

class RegisteredAuctionSpec extends AuctionSpec {
	"The auction service" should {
		"again, return 'not found' when querying the not yet existing auction state" in {
			Get((Path / "auctions" / item).toString) ~> route ~> check {
				status shouldEqual StatusCodes.NotFound
			}
		}

		"allow to create the auction" in {
			Post("/auctions", HttpEntity(`application/json`, auction.toString)) ~> route ~> check {
				status shouldEqual StatusCodes.Created
			}
		}

		"allow bidders' registration until that auction is open" in {
			Post(Uri(path = Path / "auctions" / item / "bidders"), HttpEntity(`application/json`, bidder.toString)) ~> route ~> check {
				responseAs[String] shouldEqual ""
				status shouldBe StatusCodes.OK
			}
		}

		"joining twice should not do much" in {
			Post(Uri(path = Path / "auctions" / item / "bidders"), HttpEntity(`application/json`, bidder.toString)) ~> route ~> check {
				responseAs[String] shouldEqual ""
				status shouldBe StatusCodes.OK
			}
			Get(Uri(path = Path / "bidders" / "nicolas").withQuery(Query("mine" -> "true")).toString) ~> route ~> check {
				Await.result(marshalAuctions(responseAs[String]), 1.second) shouldEqual List(Auction(auctionObject, Set(bidderObject)))
				status shouldEqual StatusCodes.OK
			}
		}

		"allow bidding as once that auction is open" in {
			setTime = Some(LocalDateTime.parse("2019-01-01T00:00:00"))
			Post(Uri(path = Path / "auctions" / item / "bids"), HttpEntity(`application/json`, bid.toString)) ~> route ~> check {
				responseAs[String] shouldEqual "" //Allows a clear report for the failing tests
				status shouldBe StatusCodes.Created
			}
		}

		"we cannot put the same bet again" in {
			Post(Uri(path = Path / "auctions" / item / "bids"), HttpEntity(`application/json`, bid.toString)) ~> route ~> check {
				status shouldBe StatusCodes.Conflict
			}
		}

		"we can put a bet beating the minimum price from the policy" in {
			Post(Uri(path = Path / "auctions" / item / "bids"), HttpEntity(`application/json`, newBid(84))) ~> route ~> check {
				responseAs[String] shouldEqual ""
				status shouldBe StatusCodes.Created
			}
		}

		"no one can join once the auction started" in {
			Post(Uri(path = Path / "auctions" / item / "bidders"), HttpEntity(`application/json`, bidder.toString)) ~> route ~> check {
				status shouldBe StatusCodes.Conflict
			}
		}

		"once closed, the auction does not accept more bets" in {
			setTime = Some(LocalDateTime.parse("2019-01-01T01:00:00"))
			Post(Uri(path = Path / "auctions" / item / "bids"), HttpEntity(`application/json`, newBid(100))) ~> route ~> check {
				status shouldBe StatusCodes.Conflict
			}
		}

		"no one can join a closed auction either" in {
			Post(Uri(path = Path / "auctions" / item / "bidders"), HttpEntity(`application/json`, bidder.toString)) ~> route ~> check {
				status shouldBe StatusCodes.Conflict
			}
		}
	}
}
