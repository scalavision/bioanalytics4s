package vcf

import scala.deriving.*
import scala.compiletime.{erasedValue, summonInline}

trait GenericDecoder[Input, Output]:
  def decode(in: Input): Output

object GenericDecoder:

  def apply[A, B](
    using da: GenericDecoder[A, B]
  ): GenericDecoder[A, B] = da

  def from[A, B](
    f: A => B
  ) = new GenericDecoder[A, B] {
    override def decode(input: A) = f(input)
  }

  given GenericDecoder[String, String] = from[String, String](identity)
  given GenericDecoder[String, Boolean] = from[String, Boolean] { s =>
    if s == "true" then true else if s == "false" then false else throw new Exception(s"Parser failed for value: $s")
  }
  given GenericDecoder[String, Short] = from[String, Short](_.toShort)
  given GenericDecoder[String, Int] = from[String, Int](_.toInt)
  given GenericDecoder[String, Long] = from[String, Long](_.toLong)
  given GenericDecoder[String, Float] = from[String, Float](_.toFloat)
  given GenericDecoder[String, Double] = from[String, Double](_.toDouble)

  given optionGenericDecoder[A](
    using d: GenericDecoder[String, A]
  ): GenericDecoder[String, Option[A]] with
    def decode(cell: String): Option[A] =
      if cell.trim.isEmpty  then Option.empty[A]
      else Some(d.decode(cell))
  
  given eitherGenericDecoder[A, B](
    using da: GenericDecoder[String, A],
          db: GenericDecoder[String, B]
  ): GenericDecoder[String, Either[A, B]] with
    def decode(cell: String): Either[A, B] =
      try { Left(da.decode(cell )) }
      catch {
        case _: Throwable => Right(db.decode(cell))
      }

  given listDecoder[A](
    using d: GenericDecoder[String, A]
  ): GenericDecoder[List[String], List[A]] with
    def decode(row: List[String]): List[A] =
      row.map(d.decode)

  given emptyTuple: GenericDecoder[List[String], EmptyTuple] with
      def decode(input: List[String]): EmptyTuple = EmptyTuple

  given tupleDecoder[A, B <: Tuple](
    using head: GenericDecoder[String, A],
          tail: GenericDecoder[List[String], B]
  ): GenericDecoder[List[String], A *: B] with
      def decode(cells: List[String]) = cells match
        case Nil => throws(s"Too few columns in Row")
        case x :: xs =>
          head.decode(x) *: tail.decode(xs)

object DecodeApi:
    import GenericDecoder.given

    def splitParser(lines: String, splitter: Char = ','): List[List[String]] =
      lines.split('\n').map(_.split(splitter).toList).toList

    def decodeUnisonRows[A](
      data: List[List[String]]
    )(
      using dec: GenericDecoder[String, A]
    ) =
      data.map(_.map(dec.decode))

    def decodeRow[A](
      data: List[String]
    )(
      using dec: GenericDecoder[List[String], A]
    ) =
      dec.decode(data)

    def decodeRowIntoTuples[A <: Tuple](
      data: List[String]
    )(
      using dec: GenericDecoder[List[String], A]
    ) =
      dec.decode(data)

    def parseAndDecodeRow[A, SplitOn](
      data: String
    )(
      using dec: GenericDecoder[List[String], A],
      splitterParser: SplitParser[SplitOn, String, List[String]]
    ) =
      val parsedData: List[String] = splitterParser.parse(data)
      dec.decode(parsedData)
    
    def decodeAllRows[A](
      data: List[List[String]]
    )(
      using dec: GenericDecoder[List[String], A]
    ) =
      data.map(dec.decode)
