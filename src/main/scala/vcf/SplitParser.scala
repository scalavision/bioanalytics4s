package vcf

trait SplitParser[Input, Splitter, Output]:
  def parse(data: Input, splitter: Splitter): Output


trait SplitterType
trait CommaSplitter extends SplitterType
trait TabSplitter extends SplitterType
trait SemiSplitter extends SplitterType
trait WhiteSpaceSplitter extends SplitterType
final case class CustomSplitter (char: Char) extends SplitterType

object SplitParser:

  def init[A,B,C](
    f: (A, B) => C
  ) = new SplitParser[A,B,C] {
    override def parse(data: A, splitter: B): C =
      f(data,splitter)
  }

  given SplitParser[Char, String, List[String]] =
    init((c, s) => s.split(c).toList)


  given SplitParser[CommaSplitter, String, List[String]] =
    init((_,s) => s.split(',').toList)

  given SplitParser[TabSplitter, String, List[String]] =
    init((_,s) => s.split('\t').toList)

  given SplitParser[WhiteSpaceSplitter, String, List[String]] =
    init((_,s) => s.split(' ').toList)

  given SplitParser[SemiSplitter, String, List[String]] =
    init((_,s) => s.split(';').toList)

  given SplitParser[CustomSplitter, String, List[String]] =
    init((customP,s) => s.split(customP.char).toList)


  def split[A](
    data: String
  )(
    using splitter: SplitParser[String, SplitterType, List[String]]
  ): List[String] = 
    splitter.parse(data, splitter)