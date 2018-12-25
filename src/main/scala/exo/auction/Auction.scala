package exo
package auction

case class Auction(
	initial: InitialAuction,
	bidders: Set[Bidder] = Set(),
	bids: List[Bid] = Nil
) {
	import Auction.State._
	import initial.parameters.{price => initialPrice, _}
	def currentPrice = bids.headOption.map(_.price)
	private def minimumBid = currentPrice.map(policy.minimumBid).getOrElse(initialPrice)
	def state = {
		val now = auction.now
		if (now.compareTo(start) < 0)
			planned
		else if (now.compareTo(end) < 0)
			started
		else
			closed
	}
	def item = initial.item
	def update(initial: InitialAuction) = {
		require(state == planned)
		copy(initial = initial)
	}
	def join(bidder: Bidder) = {
		require(state == planned, "The auction started")
		copy(bidders = bidders + bidder)
	}
	def bid(bidder: Bidder, price: Double) = {
		val state = this.state
		require(state == started, if (state == closed) "The auction is already closed" else "The auction is not open yet")
		require(bidders(bidder), "The bidder did not subscribe")
		val minimumBid = this.minimumBid
		require(price >= minimumBid, s"The minimum bid price is $minimumBid")
		copy(bids = Bid(bidder, price) :: bids)
	}
}

object Auction {
	object State extends Enumeration {
		val planned, started, closed = Value
		lazy val fromName = values.groupBy(_.toString).mapValues(_.head)
	}
	type State = State.Value
}
