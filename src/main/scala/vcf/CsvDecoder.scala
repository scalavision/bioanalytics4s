package vcf

trait RowDecoder[Row, A]:
  def decode(row: Row): A

object RowDecoder:
  import CellDecoder.given

  def apply[A, B](using da: RowDecoder[A, B]): RowDecoder[A, B] = da

  def from[A](
    f: List[String] => A
  ) = new RowDecoder[List[String], A] {
    override def decode(row: List[String]) = f(row)
  }  

  given tupleDecoder[A, B](
    using da: CellDecoder[String, A],
          db: CellDecoder[String, B]
  ): RowDecoder[List[String], (A, B)] with
    def decode(row: List[String]) = 
      ( da.decode(row(0)), db.decode(row(1)) )
  
  given listDecoder[A](
    using d: CellDecoder[String, A]
  ): RowDecoder[List[String], List[A]] with
    def decode(row: List[String]) =
      row.map(d.decode)
  
  def parseCsv(line: String): List[List[String]] = 
    line.split('\n').map(_.split(',').toList).toList

  def decodeCsv[A](input: String)(
    using rowDecoderOfA: RowDecoder[List[String], A]
  ): List[A] =
    parseCsv(input).map { rowDecoderOfA.decode }

trait CellDecoder[Cell, A]:
  def decode(cell: Cell): A
  //TODO: add an extension method here, or maybe don't

object CellDecoder:

  def apply[A, B](using da: CellDecoder[A, B]): CellDecoder[A, B] = da

  def from[A, B](
    f: A => B
  ) = new CellDecoder[A, B] {
    override def decode(cell: A) = f(cell)
  }

  given CellDecoder[String, String] = from[String, String](identity)
  given CellDecoder[String, Boolean] = from[String, Boolean](s => if s == "true" then true else false)
  given CellDecoder[String, Short] = from[String, Short](_.toShort)
  given CellDecoder[String, Int] = from[String, Int](_.toInt)
  given CellDecoder[String, Long] = from[String, Long](_.toLong)
  given CellDecoder[String, Float] = from[String, Float](_.toFloat)
  given CellDecoder[String, Double] = from[String, Double](_.toDouble)

  given optionDecoder[A](
    using d: CellDecoder[String, A]
  ): CellDecoder[String, Option[A]] with
    def decode(cell: String): Option[A] =
      if cell.trim.isEmpty  then Option.empty[A]
      else Some(d.decode(cell))
  
  given eitherCellDecoder[A, B](
    using da: CellDecoder[String, A],
          db: CellDecoder[String, B]
  ): CellDecoder[String, Either[A, B]] with
    def decode(cell: String): Either[A, B] =
      try { Left(da.decode(cell )) }
      catch {
        case _: Throwable => Right(db.decode(cell))
      }

object Decoder:

  def parseCsv(line: String): List[List[String]] = 
    line.split('\n').map(_.split(',').toList).toList

  def decodeCsv[A](
    input: String
  )(
    using decoderOfA: CellDecoder[String, A]
  ): List[List[A]] =
    parseCsv(input)
      .map(_.map(decoderOfA.decode))
