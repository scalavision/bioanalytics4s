package vcf

trait MIE[A]:
  def encode(a: A): String

