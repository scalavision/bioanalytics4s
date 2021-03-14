
def vcfParser =
  val line = "1\t2000\tref\talt\tfilter\tformat\tinfo\tgenotype"
  import vcf.VcfTransformer._
  val columns = vcfLineToColumns
  println(columns)

val csvOfIntegers = "1,2,3\n4,5,6\n"

def hetergenousTypes =
  s"""|2000,Mercury
      |1997,Ford
      |""".stripMargin

def optionalCsvData =
    s"""|2000,Mercury
        |,Ford
        |""".stripMargin

def eitherRow =
  s"""|2000,Mercury
      |true,Ford
      |""".stripMargin

def nutsData =
  s"""|1997,Ford
      |true,Mercury
      |2007,""".stripMargin
@main
def Main(args: String*): Unit =
  println(vcf.Decoder.decodeCsv[Int](csvOfIntegers))

  val data = vcf.RowDecoder.decodeCsv[(Int, String)](hetergenousTypes)
  println(data)

  val ints = vcf.RowDecoder.decodeCsv[List[Int]](csvOfIntegers)
  println(ints)

  val data2 = vcf.RowDecoder.decodeCsv[(Option[Int], String)](optionalCsvData)
  println(data2)

  val data3 = vcf.RowDecoder.decodeCsv[(Either[Int,Boolean], String)](eitherRow)
  println(data3)

  type Row = List[Either[Either[Int,Boolean],Option[String]]]
  val data4 = vcf.RowDecoder.decodeCsv[Row](nutsData)
  println(data4)
  
  println("TODO: TRY TO MERGE THE CELL AND ROW DECODER BASED UPON DIFFERENT INPUT String vs List[String]")
  println("TODO: TRY THE SCALA3 BLOG, AND CREATE CASE CLASSES")