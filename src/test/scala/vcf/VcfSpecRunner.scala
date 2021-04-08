package vcf

import zio.test._
import zio.test.Assertion._

object VcfSpecRunner extends DefaultRunnableSpec:
  override def spec: ZSpec[Environment, Failure] = 
    //StreamParserSpec.suite1
    //VcfParserSpec.suite1
    MetaInfoSpecRunner.suite1
