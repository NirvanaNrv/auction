package exo.auction

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import scala.io.StdIn

object WebServer extends App {
	implicit val system = ActorSystem("auction-web")
	implicit val materializer = ActorMaterializer()
	implicit val executionContext = system.dispatcher

	val route = new Router().route

	val bindingFuture = Http().bindAndHandle(route, "localhost", port)

	println(s"Auction server online at http://localhost:$port/\nPress <Return> to stop...")
	StdIn.readLine
	bindingFuture
		.flatMap(_.unbind)
		.onComplete(_ => system.terminate)
}
