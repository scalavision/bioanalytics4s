package vcf.encoder

trait VcfEncoder[A]:
  def encode(a: A): String

object VcfEncoder:

  def apply[A](
    using instance: VcfEncoder[A]
  ): VcfEncoder[A] = instance

  def to[A](
    f: A => String
  ): VcfEncoder[A] = new VcfEncoder {
    override def encode(a: A): String = 
      f(a)
  }

  given VcfEncoder[Int] = to(_.toString())
  given VcfEncoder[Double] = to(_.toString())
  given VcfEncoder[Long] = to(_.toString())
  given VcfEncoder[Short] = to(_.toString())
  given VcfEncoder[Float] = to(_.toString())
  given VcfEncoder[String] = to(_.toString())

  given listDecoder[A](
    using enc: VcfEncoder[A]
  ): VcfEncoder[List[A]] = to { list =>
    list.map{enc.encode}.mkString(", ")
  }

  