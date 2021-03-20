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
        case Nil => throw new Exception("empty list in decoder, not enough data provided")
        case x :: xs =>
          head.decode(x) *: tail.decode(xs)
        
  
  // given tuple2Decoder[A, B](
  //   using da: GenericDecoder[String, A],
  //         db: GenericDecoder[String, B]
  // ): GenericDecoder[List[String], (A, B)] with
  //   def decode(row: List[String]) = 
  //     ( da.decode(row(0)), db.decode(row(1)) )

  // given tuple3Decoder[A, B, C](
  //   using da: GenericDecoder[String, A],
  //         db: GenericDecoder[String, B],
  //         db: GenericDecoder[String, C],
  // ): GenericDecoder[List[String], (A, B, C)] with
  //   def decode(row: List[String]) = 
  //     ( da.decode(row(0)), db.ecode(row(1)), db.ecode(row(2)) )

  // given tuple4Decoder[A, B, C, D](
  //   using da: GenericDecoder[String, A],
  //         db: GenericDecoder[String, B],
  //         db: GenericDecoder[String, C],
  //         db: GenericDecoder[String, D],
  // ): GenericDecoder[List[String], (A, B, C, D)] with
  //   def decode(row: List[String]) = 
  //     ( da.decode(row(0)), db.ecode(row(1)), db.ecode(row(2)), db.ecode(row(3)) )

  // given tuple5Decoder[A, B, C, D, E](
  //   using da: GenericDecoder[String, A],
  //         db: GenericDecoder[String, B],
  //         db: GenericDecoder[String, C],
  //         db: GenericDecoder[String, D],
  //         db: GenericDecoder[String, E],
  // ): GenericDecoder[List[String], (A, B, C, D, E)] with
  //   def decode(row: List[String]) = 
  //     ( da.decode(row(0)), db.ecode(row(1)), db.ecode(row(2)), db.ecode(row(3)), db.ecode(row(4)))

  /* Skip the derivation part for now ... not sure how useful it is ..
  import VcfDecoder.given
  def eqSum[T](s: Mirror.SumOf[T], elems: => List[GenericDecoder[List[String], T]]): GenericDecoder[List[String], T] =
    new GenericDecoder[List[String], T]:
        def decode(input: List[String]): T =
          ???

  def eqProduct[T](p: Mirror.ProductOf[T], elems: => List[GenericDecoder[List[String], T]]): GenericDecoder[List[String], T] =
    new GenericDecoder[List[String], T]:
        def decode(input: List[String]): T =
          ???

  inline def summonAll[T <: Tuple]: List[GenericDecoder[List[String], _]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[GenericDecoder[List[String], t]] :: summonAll[ts]

  inline given derived[T](using m: Mirror.Of[T]): GenericDecoder[List[String], T] =
    lazy val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[T]     => ??? // eqSum(s, elemInstances)
      case p: Mirror.ProductOf[T] => ??? // eqProduct(p, elemInstances)
  */

object DecodeApi:
    import GenericDecoder.given
    //import VcfDecoder.given

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
