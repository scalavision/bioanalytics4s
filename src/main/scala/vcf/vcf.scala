package vcf

object throws:
  inline def apply(reason: String) = 
    throw new Exception(s"$reason")


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

import VcfFilter.*

extension [A](v1: Value[A])
  def < (v2: Value[A]) = LessThan(v1, v2)
  def > (v2: Value[A]) = LargerThan(v1, v2)
  def <= (v2: Value[A]) = Or(LessThan(v1,v2), IsEqual(v1,v2))
  def >= (v2: Value[A]) = Or(LargerThan(v1,v2), IsEqual(v1,v2))