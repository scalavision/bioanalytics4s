package vcf

import zio.test._
import zio.test.Assertion._

object VcfSpecRunner extends DefaultRunnableSpec:
  override def spec: ZSpec[Environment, Failure] = suite("VcfSpecRunner")(
    test("test"){
      assert(1)(equalTo(1))
    }
  )
