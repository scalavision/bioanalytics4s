package vcf

trait SplitDecoder[Input, A]:
  def decode(in: Input): A

object SplitDecoder:

  def apply[A, B](
    using da: SplitDecoder[A, B]
  ): SplitDecoder[A, B] = da

  def from[A, B](
    f: A => B
  ) = new SplitDecoder[A, B] {
    override def decode(input: A) = f(input)
  }

  given SplitDecoder[String, String] = from[String, String](identity)
  given SplitDecoder[String, Boolean] = from[String, Boolean](s => if s == "true" then true else false)
  given SplitDecoder[String, Short] = from[String, Short](_.toShort)
  given SplitDecoder[String, Int] = from[String, Int](_.toInt)
  given SplitDecoder[String, Long] = from[String, Long](_.toLong)
  given SplitDecoder[String, Float] = from[String, Float](_.toFloat)
  given SplitDecoder[String, Double] = from[String, Double](_.toDouble)

  given optionSplitDecoder[A](
    using d: SplitDecoder[String, A]
  ): SplitDecoder[String, Option[A]] with
    def decode(cell: String): Option[A] =
      if cell.trim.isEmpty  then Option.empty[A]
      else Some(d.decode(cell))
  
  given eitherSplitDecoder[A, B](
    using da: SplitDecoder[String, A],
          db: SplitDecoder[String, B]
  ): SplitDecoder[String, Either[A, B]] with
    def decode(cell: String): Either[A, B] =
      try { Left(da.decode(cell )) }
      catch {
        case _: Throwable => Right(db.decode(cell))
      }

  given listDecoder[A](
    using d: SplitDecoder[String, A]
  ): SplitDecoder[List[String], List[A]] with
    def decode(row: List[String]): List[A] =
      row.map(d.decode)
  
  given tupleDecoder[A, B](
    using da: SplitDecoder[String, A],
          db: SplitDecoder[String, B]
  ): SplitDecoder[List[String], (A, B)] with
    def decode(row: List[String]) = 
      ( da.decode(row(0)), db.decode(row(1)) )

object SplitParser:
    import SplitDecoder.given

    def splitParser(lines: String, splitter: Char = ','): List[List[String]] =
      lines.split('\n').map(_.split(splitter).toList).toList

    def decode[A](
      data: List[List[String]]
    )(
      using dec: SplitDecoder[String, A]
    ) =
      data.map(_.map(dec.decode))

    def decodeAll[A](
      data: List[List[String]]
    )(
      using dec: SplitDecoder[List[String], A]
    ) =
      data.map(dec.decode)
