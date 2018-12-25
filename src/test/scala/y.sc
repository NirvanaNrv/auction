import akka.http.scaladsl.model.Uri
import akka.http.scaladsl.model.Uri.Path

Uri("%2520").path.head

Uri("%2520").withFragment(" ").toString

Uri("").withPath(Path("%")).withPath(Path(" ")).toString


Path./("%")
