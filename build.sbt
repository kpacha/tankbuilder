name := "tank-builder"

organization := "com.github.kpacha"

version := "0.0.2"

scalaVersion := "2.11.7"

libraryDependencies ++={
  Seq(
    "org.scala-lang.modules" %% "scala-xml" % "1.0.5",
    "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4",
    "org.scala-lang.modules" %% "scala-swing" % "1.0.2",
    "org.scalatest" %% "scalatest" % "2.2.6" % "test"
  )
}

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
