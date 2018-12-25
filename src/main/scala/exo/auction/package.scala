package exo

import java.time.LocalDateTime

package object auction {
	val port = 5000
	val currency = "€"
	val cent = 0.01
	var setTime: Option[LocalDateTime] = None
	def now = setTime.getOrElse(LocalDateTime.now)//TODO find a way to move that to the test side
}
