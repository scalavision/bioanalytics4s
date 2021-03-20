
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
  
  val ints2 = vcf.DecodeApi.splitParser(csvOfIntegers)
  val decodedInts2 = vcf.DecodeApi.decodeUnisonRows[Int](ints2)
  println(decodedInts2)

  val decodedInts3 = vcf.DecodeApi.decodeAllRows[List[Int]](ints2)
  println(decodedInts3)

  val optionalCsvData2 = vcf.DecodeApi.splitParser(optionalCsvData)
  val data2a = vcf.DecodeApi.decodeAllRows[(Option[Int], String)](optionalCsvData2)
  println(data2a)

  val nutsData2 = vcf.DecodeApi.splitParser(nutsData)
  val nutsdataResult = vcf.DecodeApi.decodeAllRows[Row](nutsData2)
  println(nutsdataResult)

  type SingleRow = List[Either[Either[Int,Boolean],Option[String]]]
  val lines = nutsData.split('\n')
  val complexNutsData = vcf.DecodeApi.parseAndDecodeRow[SingleRow, vcf.CommaSplitter](lines.head)
  println(complexNutsData)