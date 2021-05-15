
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

def decoreDemo() =
  import vcf.decoder.*
  println(Decoder.decodeCsv[Int](csvOfIntegers))

  val data = DecodeApi.decodeCsv[(Int, String)](hetergenousTypes)
  println(data)

  val ints = DecodeApi.decodeCsv[List[Int]](csvOfIntegers)
  println(ints)

  val data2 = DecodeApi.decodeCsv[(Option[Int], String)](optionalCsvData)
  println(data2)

  val data3 = DecodeApi.decodeCsv[(Either[Int,Boolean], String)](eitherRow)
  println(data3)

  type Row = List[Either[Either[Int,Boolean],Option[String]]]
  val data4 = DecodeApi.decodeCsv[Row](nutsData)
  println(data4)
  
  val ints2 = DecodeApi.splitParser(csvOfIntegers)
  val decodedInts2 = DecodeApi.decodeUnisonRows[Int](ints2)
  println(decodedInts2)

  val decodedInts3 = DecodeApi.decodeAllRows[List[Int]](ints2)
  println(decodedInts3)

  val optionalCsvData2 = DecodeApi.splitParser(optionalCsvData)
  val data2a = DecodeApi.decodeAllRows[(Option[Int], String)](optionalCsvData2)
  println(data2a)

  val nutsData2 = DecodeApi.splitParser(nutsData)
  val nutsdataResult = DecodeApi.decodeAllRows[Row](nutsData2)
  println(nutsdataResult)

  type SingleRow = List[Either[Either[Int,Boolean],Option[String]]]
  val lines = nutsData.split('\n')
  val complexNutsData = DecodeApi.parseAndDecodeRow[SingleRow, CommaSplitter](lines.head)
  println(complexNutsData)

@main
def Main(args: String*): Unit =
  
  pprint.pprintln(doc.expr.eval1())
  pprint.pprintln(doc.expr.eval2())
  pprint.pprintln(doc.num.TestNum.result1)
