scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % "1.0.0-MF",
  "org.scalacheck" %% "scalacheck" % "1.13.4" % "test")

scalacOptions ++= Seq("-feature", "-language:postfixOps")
