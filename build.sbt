name := "tank-builder"

organization := "com.github.kpacha"

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies ++={
  Seq(
    "org.scalatest" %%  "scalatest" % "2.2.6" % "test"
  )
}

resolvers ++= Seq(
  "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"
)

scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
