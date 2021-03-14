lazy val root = project
  .in(file("."))
  .settings(
    name := "vcfprocessor",
    description := "Example sbt project that compiles using Scala 3",
    version := "0.1.0",

    scalaVersion := "3.0.0-RC1",

    useScala3doc := true,
  )
