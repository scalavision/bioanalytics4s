package vcf

trait SplitParser[Splitter, Input, Output]:
  def parse(data: Input): Output


trait SplitterType
trait CommaSplitter extends SplitterType
trait TabSplitter extends SplitterType
trait SemiSplitter extends SplitterType
trait WhiteSpaceSplitter extends SplitterType

// TODO: create an inline TYPE matcher, it should actually work
//       only doing this at compile time, will mean there is no
//       overhead ..

object SplitParser:

  def init[Splitter, A, B](
    f: A => B
  ) = new SplitParser[Splitter, A,B] {
    override def parse(data: A): B =
      f(data)
  }

  given SplitParser[CommaSplitter, String, List[String]] =
    init((s) => s.split(',').toList)

  given SplitParser[TabSplitter, String, List[String]] =
    init((s) => s.split('\t').toList)

  given SplitParser[WhiteSpaceSplitter, String, List[String]] =
    init((s) => s.split(' ').toList)

  given SplitParser[SemiSplitter, String, List[String]] =
    init((s) => s.split(';').toList)

  // given SplitParser[CustomSplitter, String, List[String]] with
  //   extension[](c: CustomSplitter) def parse(data: String): List[String] =
  //     data.split(c.char).toList

  def split[SplitterType](
    data: String
  )(
    using splitter: SplitParser[SplitterType, String, List[String]]
  ): List[String] = 
    splitter.parse(data)