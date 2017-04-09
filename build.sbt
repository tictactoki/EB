lazy val root = (project in file("."))
  .settings(
  name := "EB",
    scalaVersion := "2.12.1"
)

fork in run := true

javacOptions ++= Seq(
  "-Xmx:1G"
)

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
