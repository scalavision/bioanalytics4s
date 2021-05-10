package vcf

enum VcfFilter [+A] { self =>

  case IsEqual[A](value: A) extends VcfFilter[A]
  case LessThan[A](value: A) extends VcfFilter[A]
  case LessThanOrEqual[A](value: A) extends VcfFilter[A]
  case LargerThan[A](value: A) extends VcfFilter[A]
  case LargerThanOrEqual[A](value: A) extends VcfFilter[A]
  case OneOf[A](values: List[A]) extends VcfFilter[A]
  case And(left: VcfFilter[A], right: VcfFilter[A]) extends VcfFilter[A]
  case Or(left: VcfFilter[A], right: VcfFilter[A]) extends VcfFilter[A]
  case XOr(left: VcfFilter[A], right: VcfFilter[A]) extends VcfFilter[A]
  case Not(filter: VcfFilter[A])

  def &&[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.And[A2](self, that)

  def ||[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.Or[A2](self, that)

  // https://docs.scala-lang.org/tour/operators.html
  def xor[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.XOr(self, that)
    //(self || that) && !(self && that)

  def unary_! : VcfFilter[A] = VcfFilter.Not[A](self)

  def ===[A2 >: A] (a: A2) = VcfFilter.IsEqual(a)

  def <= [A2 >: A] (a: A2) = VcfFilter.LessThanOrEqual(a)
  def >= [A2 >: A] (a: A2) = VcfFilter.LargerThanOrEqual(a)
  def < [A2 >: A] (a: A2) = VcfFilter.LessThan(a)
  def > [A2 >: A] (a: A2) = VcfFilter.LargerThan(a)

  def oneOf[A2 >: A](as: A2*) = VcfFilter.OneOf(as.toList)
}

object VcfFilter:

  def range(start: Chrom, posStart: Pos, end: Chrom, posEnd: Pos) = 
    LargerThan(start) && LargerThan(posStart) && LessThan(end) && LessThan(posEnd)
  