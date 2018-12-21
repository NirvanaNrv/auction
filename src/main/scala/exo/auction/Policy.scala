package exo.auction

sealed trait Policy extends Product {
	def minimumBid(price: Double): Double
}

object Policy {
	case class Constant(value: Double) extends Policy {
		require(value >= cent)
		def minimumBid(price: Double) = price + value
	}
	def apply(increment: Double): Policy = Constant(increment)
}
