package vcf

trait ADTPrinter[A]:
  def print(a: A): String

object ADTPrinter:
  import scala.deriving.Mirror

  def apply[A](instance: ADTPrinter[A]): ADTPrinter[A] = instance

  def to[A](f: A => String): ADTPrinter[A] = new ADTPrinter:
    def print(a: A): String = f(a)
  
  /**
   * Remark that givens for every type of the ADT is required
   * for this to work
   */
  given ADTPrinter[Int] = to(_.toString)
  given ADTPrinter[Double] = to(_.toString)
  given ADTPrinter[String] = to(identity)

  inline def adtTrait[A](using m: Mirror.SumOf[A]): ADTPrinter[A] =
    new ADTPrinter[A] {
      def print(a: A): String =
        // val label = labelFromMirror[m.MirroredType] - not needed
        // val elemLabels = getElemLabels[m.MirroredElemLabels] - not needed
        val elemInstances = auto.getTypeclassInstances[ADTPrinter, m.MirroredElemTypes] // same as for the case class
        val elemOrdinal = m.ordinal(a) // Checks the ordinal of the type, e.g. 0 for User or 1 for AnonymousVisitor
    
        // just return the result of prettyString from the right element instance
        elemInstances(elemOrdinal).print(a)
    }
  
  inline def adtProduct[A](using m: Mirror.ProductOf[A]): ADTPrinter[A] =
    import auto.*
    new ADTPrinter[A]{
      def print(a: A): String =
        val label = labelFromMirror[m.MirroredType]
        val elemLabels = getElemLabels[m.MirroredElemLabels]
        val elemInstances = getTypeclassInstances[ADTPrinter, m.MirroredElemTypes]
        val elems = a.asInstanceOf[Product].productIterator
        val elemString = elems.zip(elemLabels).zip(elemInstances).map {
          case ((elem, label), instance) =>
            s"${label}=${instance.print(elem)}"
        }

        if(elemLabels.isEmpty) then
          label
        else
          s"$label(${elemString.mkString(", ")})"
    }

  inline given derived[A](using m: Mirror.Of[A]): ADTPrinter[A] =
    inline m match
      //case c: doc.shapeit.Token.TCommand => to(_ => "tcommand")
      case s: Mirror.SumOf[A] => adtTrait(using s)
      case p: Mirror.ProductOf[A] => adtProduct(using p)