val scala3Version = "3.7.2"

lazy val root = project
  .in(file("."))
  .settings(
    name                                   := "nop",
    version                                := "0.1.0-SNAPSHOT",
    scalaVersion                           := scala3Version,
    coverageEnabled                        := true,
    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.19",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
  )
