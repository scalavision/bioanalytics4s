package examples

object ConsoleApp:

  trait Free[F[_], A]

  enum Console[A]:
    case ReadLine[A](value: String => A) extends Console[A]
    case PrintLine[A](line: String, value: A) extends Console[A]

  type Dsl[A] = Free[Console, A]

  // import Console._

  // def readLine  : Dsl[String]             = ReadLine(identity)
  // def printLine(line: String): Dsl[Unit]  = PrintLine(line, ())
  
