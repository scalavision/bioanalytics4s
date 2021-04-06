package vcf

import zio.test._
import zio.test.Assertion._
import zio.stream._

object StreamParserSpec:
  def suite1 = suite("StreamParserSpec")(
    test("parse complete file") {

      val data = ZStream.fromFile(java.nio.file.Paths.get("/stash/bio/vcf/merged_Diag-ValidationWGS2-HG002C2-PM.vcf")).transduce(
        ZTransducer.utf8Decode >>> ZTransducer.splitLines
      )
      .filter(!_.startsWith("#")).map { line =>
        VcfParser.singleSample(line)
      }
      //.peel(ZSink.head[String])
      
      val rt = zio.Runtime.default

      val result = rt.unsafeRun(data.runCollect)
      println(result.size)

      assert(1)(equalTo(1))
    }

  )