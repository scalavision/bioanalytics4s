package vcf
import vcf.decoder.{DecodeApi, GenericDecoder}



object VcfDecoder:
  import GenericDecoder.from
  given GenericDecoder[String, Chrom] = from[String, Chrom](Chrom.apply)
  given GenericDecoder[String, Pos] = from[String, Pos](s => Pos(s.toInt))
  given GenericDecoder[String, Ref] = from[String, Ref](s => Ref(s))
  given GenericDecoder[String, Alt] = from[String, Alt](s => Alt(s))
  given GenericDecoder[String, Qual] = from[String, Qual](s => Qual(s))
  given GenericDecoder[String, SampleId] = from[String, SampleId](s => SampleId(s))
  given GenericDecoder[String, List[SampleId]] = from[String, List[SampleId]](s => s.split(';').toList.map(SampleId.apply))
  given GenericDecoder[String, Filter] = from[String, Filter]( s => Filter(s.split(':').toList))
  given GenericDecoder[String, Genotype] = from[String, Genotype](s => Genotype.apply(s.split(':').toList))
  given GenericDecoder[List[String], List[Genotype]] = from[List[String], List[Genotype]](s => s.map(s => Genotype.apply(s.split(':').toList)))
  given GenericDecoder[String, Info] = from[String, Info] { s =>
    val infoMap: Map[String, List[String]] = s.split(';').toList.foldLeft(Map.empty[String, List[String]]){(acc, s) =>
      val keyValues = s.split('=')
      acc + (keyValues.head -> keyValues.tail.mkString.split(',').toList)
    }
    Info(infoMap)
  }

  given GenericDecoder[String, Format] = from[String, Format]( s => Format(s.split(':').toList))
  
object VcfParser:
  import VcfDecoder.given
  import scala.deriving.*
  
  def singleSample(vcfLine: String): Vcf.SingleSample =
    val parsedResult = DecodeApi.decodeRowIntoTuples[(Chrom, Pos, SampleId, Ref, Alt, Qual, Filter, Info, Format, Genotype)](vcfLine.split('\t').toList)
    summon[Mirror.Of[vcf.Vcf.SingleSample]].fromProduct(parsedResult)
