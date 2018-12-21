package exo.auction

import java.time.LocalDateTime

case class AuctionParameters(
	auctioneer: Auctioneer,
	price: Double,
	policy: Policy,
	start: LocalDateTime,
	end: LocalDateTime
) {
	//require(price > cent)
	require(start.compareTo(end) < 0)
}
