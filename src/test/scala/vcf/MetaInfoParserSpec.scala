package vcf

import zio.test.*
import zio.test.Assertion.*

object MetaInfoSpecRunner:

  val info1 = """##INFO=<ID=END,Number=1,Type=Integer,Description="End position of the structural variant">"""
  val info2 = """##INFO=<ID=END,Number=1,Type=Integer,Description="End position of the structural variant", "source"="article_ref", "version"="1.6">"""
  val info3 = """##INFO=<ID=END,Number=1,Type=Integer,Description="End position of the structural variant", "source"="article_ref", "version"="1.6", "some_key"="some_value">"""
  val format1 = """##FORMAT=<ID=PB_REF,Number=1,Type=Integer,Description="Number of PacBio reads supporting the REF allele as predicted by svviz">"""
  val format2 = """##FORMAT=<ID=PB_REF,Number=1,Type=Integer,Description="Number of PacBio reads supporting the REF allele as predicted by svviz", source"="article_ref", "version"="1.6", "some_key"="some_value">"""
  val contig1 = """##contig=<ID=1,length=249250621>"""
  val unknown = """##unknown=<ID=1,info="hello">""""
  val fileformat1 = """##fileformat=VCFv4.2"""
  val fileDate1="""##fileDate=20180605"""

  val suite1 = suite("meta info parser")(
    test("info field"){
      import MetaInfo.*
      import NumberType.*
      import DataType.*

      val info = INFO("END",Length(1),Integer,"\"End position of the structural variant\"",None,None,Map())
      val parsedInfo = MetaInfo(info1)
      val parsedInfo2 = MetaInfo(info2)
      val info2Target =
        INFO("END",Length(1),Integer,"\"End position of the structural variant\"",source = Some("\"article_ref\""),version = Some("\"1.6\""),Map())
      val parsedInfo3 = MetaInfo(info3)
      val info3Target =
        INFO(
          id = "END",
          nrOfValues = Length(value = 1),
          tpe = Integer,
          description = "\"End position of the structural variant\"",
          source = Some(value = "\"article_ref\""),
          version = Some(value = "\"1.6\""),
          additionalFields = Map(" \"some_key\"" -> "\"some_value\"")
        )
      val parsedFormat1 = MetaInfo(format1)
      val parsedFormat2 = MetaInfo(format2)
      val targetFormat1 = FORMAT("PB_REF",Length(1), Integer,"\"Number of PacBio reads supporting the REF allele as predicted by svviz\"",Map()) 
      val targetFormat2 = FORMAT("PB_REF",Length(1), Integer,"\"Number of PacBio reads supporting the REF allele as predicted by svviz\"",Map(
        " source\"" -> "\"article_ref\"",
        " \"version\"" -> "\"1.6\"",
        " \"some_key\"" -> "\"some_value\""
        )
      )
      val parsedConfig1 =  MetaInfo(contig1)
      val targetConfig1 = Contig(id = "1", length = Some(value = 249250621), additionalFields = Map())

      val parsedFileFormat = MetaInfo(fileformat1)
      val targetFileFormat = FileFormat("VCFv4.2")

      assert(info)(equalTo(parsedInfo)) &&
      assert(parsedInfo2)(equalTo(info2Target)) &&
      assert(parsedInfo3)(equalTo(info3Target)) &&
      assert(parsedInfo3)(equalTo(info3Target)) &&
      assert(parsedFormat1)(equalTo(targetFormat1)) &&
      assert(parsedFormat2)(equalTo(targetFormat2)) &&
      assert(parsedConfig1)(equalTo(targetConfig1)) &&
      assert(parsedFileFormat)(equalTo(targetFileFormat))

      // &&
      // assert(INFO("END",Length(1),Integer,"End position of the structural variant",None,None,Map()))(equalTo(MetaInfo(format1)))
      
    }
  )
