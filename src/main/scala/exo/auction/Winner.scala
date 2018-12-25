package exo.auction

/**
	* Just a json message to fetch the final price
	*
	* @param bidder
	* @param price
	*/
case class Winner(bidder: String, price: Double)
