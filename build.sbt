lazy val zioVersion = "1.0.6"
lazy val OSLibVersion = "0.7.4"
val PPrintVersion = "0.6.4"
val SourcecodeVersion = "0.2.5"

lazy val root = project
  .in(file("."))
  .settings(
    name := "vcfprocessor",
    description := "Example sbt project that compiles using Scala 3",
    version := "0.1.0",

    scalaVersion := "3.0.0-RC2",

    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "com.lihaoyi" %% "sourcecode" % SourcecodeVersion,
      "com.lihaoyi" %% "pprint" % PPrintVersion,
      "com.lihaoyi" %% "os-lib" % OSLibVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
      "org.scalameta" %% "munit" % "0.7.23" % Test,
    ),
    testFrameworks ++= Seq(
      new TestFramework("munit.Framework"),
      new TestFramework("zio.test.sbt.ZTestFramework")
    )
  )
