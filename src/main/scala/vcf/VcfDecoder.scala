package vcf

enum Vcf:
  case SingleSample(
    chrom: Chrom,
    pos: Pos,
    id: SampleId,
    ref: Ref,
    alt: Alt,
    qual: Qual,
    filter: Filter,
    info: Info,
    format: Format,
    genotypes: Genotype
  )

  case MultiSample(
    chrom: Chrom,
    pos: Pos,
    id: List[SampleId],
    ref: Ref,
    alt: Alt,
    qual: Qual,
    filter: Filter,
    info: Info,
    format: Format,
    genotypes: List[Genotype]
  )

sealed trait VcfType
final case class Chrom(value: String) extends VcfType
final case class Pos(value: Int) extends VcfType
final case class SampleId(value: String) extends VcfType
final case class Ref(value: String) extends VcfType
final case class Alt(value: String) extends VcfType
final case class Qual(value: String) extends VcfType
final case class Filter(values: List[String]) extends VcfType
final case class Info(values: Map[String, List[String]]) extends VcfType
final case class Format(values: List[String]) extends VcfType
final case class Genotype(s: List[String])

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
