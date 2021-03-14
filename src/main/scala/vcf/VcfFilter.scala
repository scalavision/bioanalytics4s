package vcf

enum VcfFilter [+A] { self =>
  case And(left: VcfFilter[A], right: VcfFilter[A])
  case Or(left: VcfFilter[A], right: VcfFilter[A])
  case Not(filter: VcfFilter[A])     

  def &&[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.And[A2](self, that)

  def ||[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = VcfFilter.Or[A2](self, that)

  // https://docs.scala-lang.org/tour/operators.html
  def xor[A2 >: A](that: VcfFilter[A2]): VcfFilter[A2] = 
    (self || that) && !(self && that)

  def unary_! : VcfFilter[A] = VcfFilter.Not[A](self)

}
