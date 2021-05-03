package vcf.decoder

trait MIE[A]:
  def encode(a: A): String

object MIE:
  import scala.deriving.Mirror

  def apply[A](instance: MIE[A]): MIE[A] = instance

  def to[A](f: A => String): MIE[A] = new MIE:
    def encode(a: A): String = f(a)

  given MIE[Int] = to(_.toString)
  given MIE[Double] = to(_.toString)
  given MIE[String] = to(identity)

  inline def mieTrait[A](using m: Mirror.SumOf[A]): MIE[A] =
    new MIE[A] {
      def encode(a: A): String =
        // val label = labelFromMirror[m.MirroredType] - not needed
        // val elemLabels = getElemLabels[m.MirroredElemLabels] - not needed
        val elemInstances = auto.getTypeclassInstances[MIE, m.MirroredElemTypes] // same as for the case class
        val elemOrdinal = m.ordinal(a) // Checks the ordinal of the type, e.g. 0 for User or 1 for AnonymousVisitor
    
        // just return the result of prettyString from the right element instance
        elemInstances(elemOrdinal).encode(a)
    }
  
  inline def mieProduct[A](using m: Mirror.ProductOf[A]): MIE[A] =
    import auto.*
    new MIE[A]{
      def encode(a: A): String =
        val label = labelFromMirror[m.MirroredType]
        val elemLabels = getElemLabels[m.MirroredElemLabels]
        val elemInstances = getTypeclassInstances[MIE, m.MirroredElemTypes]
        val elems = a.asInstanceOf[Product].productIterator
        val elemString = elems.zip(elemLabels).zip(elemInstances).map {
          case ((elem, label), instance) =>
            s"${label}=${instance.encode(elem)}"
        }

        if(elemLabels.isEmpty) then
          label
        else
          s"$label(${elemString.mkString(", ")})"
    }

  inline given derived[A](using m: Mirror.Of[A]): MIE[A] =
    inline m match
      //case c: doc.shapeit.Token.TCommand => to(_ => "tcommand")
      case s: Mirror.SumOf[A] => mieTrait(using s)
      case p: Mirror.ProductOf[A] => mieProduct(using p)
  