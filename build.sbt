val scala3Version = "3.1.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "lab1",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies += "org.scalameta" %% "munit" % "0.7.29" % Test,
    libraryDependencies += "org.scalameta" %% "munit-scalacheck" % "0.7.29" % Test
    // libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.12",
    // libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.12" % "test",
    // libraryDependencies += "org.scalatest" %% "scalatest-propspec" % "3.2.12" % "test"
  )
