package vcf

case class Vcf(
  chrom: Chrom,
  pos: Pos,
  ref: Ref,
  alt: Alt,
  qual: Qual,
  filter: Filter,
  info: Info,
  format: Option[Format],
  genotypes: List[String]
)

sealed trait VcfType
final case class Chrom(value: String) extends VcfType
final case class Pos(value: Int) extends VcfType
final case class Ref(value: String) extends VcfType
final case class Alt(value: String) extends VcfType
final case class Qual(value: String) extends VcfType
final case class Filter(values: List[String]) extends VcfType
final case class Info(values: Map[String, List[String]]) extends VcfType
final case class Format(values: List[String]) extends VcfType
final case class Genotypes(values: List[String]) extends VcfType

object VcfDecoder:
  import GenericDecoder.from
  given GenericDecoder[String, Chrom] = from[String, Chrom](Chrom.apply)
  given GenericDecoder[String, Pos] = from[String, Pos](s => Pos.apply(s.toInt))
  given GenericDecoder[String, Ref] = from[String, Ref](s => Ref(s))
  given GenericDecoder[String, Alt] = from[String, Alt](s => Alt(s))
  given GenericDecoder[String, Qual] = from[String, Qual](s => Qual(s))
  given GenericDecoder[String, Filter] = from[String, Filter]( s => Filter(s.split(':').toList))
    
  given GenericDecoder[String, Info] = from[String, Info] { s =>
    val infoMap: Map[String, List[String]] = s.split(';').toList.foldLeft(Map.empty[String, List[String]]){(acc, s) =>
      val keyValues = s.split('=')
      acc + (keyValues.head -> keyValues.tail.mkString.split(',').toList)
    }
    Info(infoMap)
  }

  given GenericDecoder[String, Format] = from[String, Format]( s => Format(s.split(':').toList))
  given GenericDecoder[String, Genotypes] = from[String, Genotypes]( s => Genotypes(s.split(':').toList))
  given GenericDecoder[List[String], Vcf](
    using dec: GenericDecoder[List[String], VcfType]
  ): GenericDecoder[List[String], VcfType] = from[String, Vcf]{ s => 
    ???
  }
  
  
object VcfParser:
  import GenericDecoder.given
  import VcfDecoder.given

  // private def parser(cols: List[String])(
  //   // using dec: GenericDecoder[List[String], (Chrom, Pos, Ref, Alt, Qual, Filter, Info, Option[Format], List[String])]
  //   // we need a hetergenous list, or tuple machinery
  //   // Look into the mirror blog, you should be able to split the case class product into its elements
  //   // then recursively parse each element individually, even in parallel in theory, but not worth the work atm
  //   using dec: GenericDecoder[List[String], Vcf]
  // ) =
  //   dec.decode(cols)

  def line(s: String): Vcf =
    val cols = s.split('\t').toList
    //parser(cols)
    ???
