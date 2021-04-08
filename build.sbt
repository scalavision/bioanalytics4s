lazy val zioVersion = "1.0.5"

lazy val root = project
  .in(file("."))
  .settings(
    name := "vcfprocessor",
    description := "Example sbt project that compiles using Scala 3",
    version := "0.1.0",

    scalaVersion := "3.0.0-RC2",

    //useScala3doc := true,
    
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
      "org.scalameta" %% "munit" % "0.7.22" % Test,

      
    ),
    testFrameworks ++= Seq(
      new TestFramework("munit.Framework"),
      new TestFramework("zio.test.sbt.ZTestFramework")
    )
  )
