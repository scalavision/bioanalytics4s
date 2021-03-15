package vcf

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
    if s == "true" then true else false
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
  
  given tupleDecoder[A, B](
    using da: GenericDecoder[String, A],
          db: GenericDecoder[String, B]
  ): GenericDecoder[List[String], (A, B)] with
    def decode(row: List[String]) = 
      ( da.decode(row(0)), db.decode(row(1)) )

object SplitParser:
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
    
    def decodeAllRows[A](
      data: List[List[String]]
    )(
      using dec: GenericDecoder[List[String], A]
    ) =
      data.map(dec.decode)
