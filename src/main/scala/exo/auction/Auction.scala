package exo.auction

import java.time.LocalDateTime

case class Auction(
	initial: InitialAuction,
	bidders: Set[Bidder] = Set(),
	bids: List[Bid] = Nil
) {
	import Auction.State._
	import initial.parameters.{price => initialPrice, _}
	def currentPrice = bids.lastOption.map(_.price)
	private def minimumBid = currentPrice.map(policy.minimumBid).getOrElse(initialPrice)
	def state = {
		val now = LocalDateTime.now
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
		require(state == planned)
		copy(bidders = bidders + bidder)
	}
	def bid(bidder: Bidder, price: Double) = {
		require(state == started)
		require(bidders(bidder))
		require(price >= minimumBid)
		copy(bids = Bid(bidder, price) :: bids)
	}
}

object Auction {
	object State extends Enumeration {
		val planned, started, closed = Value
	}
	type State = State.Value
}
