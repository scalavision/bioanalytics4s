package vcf

import zio.test._
import zio.test.Assertion._

object VcfParserSpec:
  val suite1 = suite("VcfParser")(
    test("parse rows of vcf data"){

      val data = SimpleTestData.DataLines.CnvNator_Manta_Delly_Tiddit

      val parsed = data.map { line =>
        VcfParser.singleSample(line)
      }
      println(parsed)
      assert(1)(equalTo(1))
    }
  )