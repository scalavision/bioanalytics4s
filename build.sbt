lazy val zioVersion = "1.0.7"
lazy val OSLibVersion = "0.7.6"
val PPrintVersion = "0.6.5"
val SourcecodeVersion = "0.2.6"
val uJsonVersion = "1.3.12"
val munitVersion = "0.7.25"
val uPickleVersion = "1.3.12"

lazy val root = project
  .in(file("."))
  .settings(
    name := "vcfprocessor",
    description := "Basic bioinformatics in Scala",
    version := "0.1.0",

    scalaVersion := "3.0.0-RC3",

    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % zioVersion,
      "dev.zio" %% "zio-streams" % zioVersion,
      "com.lihaoyi" %% "sourcecode" % SourcecodeVersion,
      "com.lihaoyi" %% "pprint" % PPrintVersion,
      "com.lihaoyi" %% "os-lib" % OSLibVersion,
      "com.lihaoyi" %% "upickle" % uPickleVersion,
      "dev.zio" %% "zio-test" % zioVersion % Test,
      "dev.zio" %% "zio-test-sbt" % zioVersion % Test,
      "org.scalameta" %% "munit" % munitVersion % Test,
    ),
    testFrameworks ++= Seq(
      new TestFramework("munit.Framework"),
      new TestFramework("zio.test.sbt.ZTestFramework")
    )
  )
