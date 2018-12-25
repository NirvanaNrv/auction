name := "auction"

version := "0.1"

scalaVersion := "2.12.8"

val akkaVersion = "2.5.19"
libraryDependencies += "com.typesafe.akka" %% "akka-http"   % "10.1.5"
//libraryDependencies += "com.typesafe.akka" %% "akka-stream" % akkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-stream-typed" % akkaVersion
libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.5"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % Test
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % Test
libraryDependencies += "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test
libraryDependencies += "com.typesafe.akka" %% "akka-http-testkit" % "10.1.6" % Test

parallelExecution in Test := false
