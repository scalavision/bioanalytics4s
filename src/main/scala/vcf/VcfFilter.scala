package vcf

/*
We are mixing filtering with Ordering which is a different concern
*/

trait Region[A]:
  def start: A
  def end: A

//trait Ord[A]:

enum VcfFilter [+A] { self =>

  case Value(a: A) extends VcfFilter[A]
  // should also add LessThan, LargerThan, IsEqual etc.
  case And(left: VcfFilter[A], right: VcfFilter[A]) extends VcfFilter[A]
  case Or(left: VcfFilter[A], right: VcfFilter[A]) extends VcfFilter[A]
  case XOr(left: VcfFilter[A], right: VcfFilter[A]) extends VcfFilter[A]
  case Not(filter: VcfFilter[A])
  case IsEqual(v1: Value[A], v2: Value[A]) extends VcfFilter[A]
  case LessThan(v1: Value[A], v2: Value[A]) extends VcfFilter[A]
  case LargerThan(v1: Value[A], v2: Value[A]) extends VcfFilter[A]

  def &&[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.And(self, that)
  def ||[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.Or(self, that)
  // https://docs.scala-lang.org/tour/operators.html
  def xor[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.XOr(self, that)
  def unary_! : VcfFilter[A] = VcfFilter.Not(self)

}

import VcfFilter.*

extension[A] (v1: Value[A])
  def < (v2: Value[A]): VcfFilter[A] = LessThan(v1, v2)
  def > (v2: Value[A]): VcfFilter[A] = LargerThan(v1, v2)
  def <= (v2: Value[A]): VcfFilter[A] = Or(LessThan(v1,v2), IsEqual(v1,v2))
  def >= (v2: Value[A]): VcfFilter[A] = Or(LargerThan(v1,v2), IsEqual(v1,v2))

object VcfFilter:

  def range(pos: (Chrom, Pos), start: (Chrom,Pos), end: (Chrom,Pos)): VcfFilter[VcfType] = 
    Value(start._1) < Value(pos._1) && Value(start._2) < Value(pos._2) &&
      Value(end._1) > Value(pos._1) && Value(end._2) > Value(pos._2)

  //TODO: find a way to make this compile
  // def rangeExclusive[A: Comparison](value: A, region: Region[A]) = ???
  // def rangeExclusive[A: Comparison](value: A, region: Region[A]) = ???
  def rangeExclusive[A: Comparison](value: A, region: Region[A]) = ???
  
    //Value(value > region.start) && Value(value < region.stop)

  //   LargerThan(start) && LargerThan(posStart) && LessThan(end) && LessThan(posEnd)

  // def rangeInclusive(start: Chrom, posStart: Pos, end: Chrom, posEnd: Pos): VcfFilter[VcfType] = 
  //   LargerThan(start) && LargerThan(posStart) && LessThan(end) && LessThan(posEnd)
  
  
