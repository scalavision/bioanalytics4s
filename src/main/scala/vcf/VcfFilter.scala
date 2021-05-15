package vcf

/*
We are mixing filtering with Ordering which is a different concern
*/

trait Region[A]{
  def start: A
  def end: A
}

trait Comparison[A] extends Ordered[A] {
  def isEqual(a1: A, a2: A): Boolean
  def > (a1: A, a2: A): Boolean
  def < (a1: A, a2: A): Boolean
}

enum VcfFilter [+A] { self =>

  case Value(b: Boolean) extends VcfFilter[A]
  // should also add LessThan, LargerThan, IsEqual etc.
  case And(left: VcfFilter[A], right: VcfFilter[A]) extends VcfFilter[A]
  case Or(left: VcfFilter[A], right: VcfFilter[A]) extends VcfFilter[A]
  case XOr(left: VcfFilter[A], right: VcfFilter[A]) extends VcfFilter[A]
  case Not(filter: VcfFilter[A])
  case LessThan(v1: Value[A], v2: Value[A])
  def &&[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.And(self, that)

  def ||[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.Or(self, that)

  // https://docs.scala-lang.org/tour/operators.html
  def xor[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.XOr(self, that)
    //(self || that) && !(self && that)

  def unary_! : VcfFilter[A] = VcfFilter.Not(self)

}

import VcfFilter.*

extension [A](v1: Value[A])
  def < (v2: Value[A]) = VcfFilter.LessThan(v1, v2)
  
//TODO: Make implicit conversions for VcfType to VcfFilter.Value

object VcfFilter:

  //TODO: find a way to make this compile
  def rangeExclusive[A: Comparison](value: A, region: Region[A]) = ???
    
    //Value(value > region.start) && Value(value < region.stop)

  //   LargerThan(start) && LargerThan(posStart) && LessThan(end) && LessThan(posEnd)

  // def rangeInclusive(start: Chrom, posStart: Pos, end: Chrom, posEnd: Pos): VcfFilter[VcfType] = 
  //   LargerThan(start) && LargerThan(posStart) && LessThan(end) && LessThan(posEnd)
  
  