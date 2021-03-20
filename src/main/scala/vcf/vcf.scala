package vcf

object throws:
  inline def apply(reason: String) = 
    throw new Exception(s"$reason")

