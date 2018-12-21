val max = 100000
val range = 1 to max
val it = range.zipWithIndex.toMap.values

def time(body: =>Unit): Long = {
	val start = System.nanoTime
	body
	System.nanoTime - start
}

time(range.toList.filter(_ < max / 2))
time(it.toList.filter(_ < max / 2))
