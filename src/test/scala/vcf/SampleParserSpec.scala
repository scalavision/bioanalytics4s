package vcf

import zio.test.*
import zio.test.Assertion.*
import upickle.default.*

object SampleParserSpec:

  val t1 = """|{
              |  "Diag-wgs11-HG002" : {
              |     "cnv_vcf" : "/some/path/to/merged.vcf",
              |     "bam" : "/some/path/to/file.bam"
              }
              |}""".stripMargin

  val suite1 = suite("sample parser")(
    test("basic json object"){
      val sampleJson = ujson.read(t1)
      pprint.pprintln(sampleJson)
      val sampleObj = sampleJson.obj
      val sample = sampleObj("Diag-wgs11-HG002").obj

      sample.contains("cnv_vcf")
      pprint.pprintln(sample)

      assert(10)(equalTo(10))
    }
  )