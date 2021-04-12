package vcf

import scala.deriving.Mirror
import scala.compiletime.{constValue, erasedValue, summonInline}

/**************************** trait to be used for simple derivation below **************************/

// this traits can just be copy/pasted or reside in a library
trait EasyDerive[TC[_]] {
  final def apply[A](using tc: TC[A]): TC[A] = tc

  case class CaseClassElement[A, B](label: String, typeclass: TC[B], getValue: A => B, idx: Int)
  case class CaseClassType[A](label: String, elements: List[CaseClassElement[A, _]], fromElements: List[Any] => A)

  case class SealedElement[A, B](label: String, typeclass: TC[B], idx: Int, cast: A => B)
  case class SealedType[A](label: String, elements: List[SealedElement[A, _]], getElement: A => SealedElement[A, _])

  inline def getInstances[A <: Tuple]: List[TC[Any]] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[TC[t]].asInstanceOf[TC[Any]] :: getInstances[ts]
  }

  inline def getElemLabels[A <: Tuple]: List[String] = inline erasedValue[A] match {
    case _: EmptyTuple => Nil
    case _: (t *: ts) => constValue[t].toString :: getElemLabels[ts]
  }

  def deriveCaseClass[A](caseClassType: CaseClassType[A]): TC[A]

  def deriveSealed[A](sealedType: SealedType[A]): TC[A]

  inline given derived[A](using m: Mirror.Of[A]): TC[A] = {
    val label = constValue[m.MirroredLabel]
    val elemInstances = getInstances[m.MirroredElemTypes]
    val elemLabels = getElemLabels[m.MirroredElemLabels]

    inline m match {
      case s: Mirror.SumOf[A] =>
        val elements = elemInstances.zip(elemLabels).zipWithIndex.map{ case ((inst, lbl), idx) =>
          SealedElement[A, Any](lbl, inst.asInstanceOf[TC[Any]], idx, identity)
        }
        val getElement = (a: A) => elements(s.ordinal(a))
        deriveSealed(SealedType[A](label, elements, getElement))

      case p: Mirror.ProductOf[A] =>
        val caseClassElements =
          elemInstances
            .zip(elemLabels)
            .zipWithIndex.map{ case ((inst, lbl), idx) =>
              CaseClassElement[A, Any](lbl, inst.asInstanceOf[TC[Any]],
                (x: Any) => x.asInstanceOf[Product].productElement(idx), idx)
            }
        val fromElements: List[Any] => A = { elements =>
          val product: Product = new Product {
            override def productArity: Int = caseClassElements.size

            override def productElement(n: Int): Any = elements(n)

            override def canEqual(that: Any): Boolean = false
          }
          p.fromProduct(product)
        }
        deriveCaseClass(CaseClassType[A](label, caseClassElements, fromElements))
    }
  }
}

/**************************** define typeclass and derivation **************************/

trait PrettyString[A] {
  def prettyString(a: A): String
}

object PrettyString extends EasyDerive[PrettyString] {
  override def deriveCaseClass[A](productType: CaseClassType[A]): PrettyString[A] = new PrettyString[A] {
    override def prettyString(a: A): String = {
      if (productType.elements.isEmpty) productType.label
      else {
        val prettyElements = productType.elements.map(p => s"${p.label}=${p.typeclass.prettyString(p.getValue(a))}")
        prettyElements.mkString(s"${productType.label}(", ", ", ")")
      }
    }
  }

  override def deriveSealed[A](sumType: SealedType[A]): PrettyString[A] = new PrettyString[A] {
    override def prettyString(a: A): String = {
      val elem = sumType.getElement(a)
      elem.typeclass.prettyString(elem.cast(a))
    }
  }

  // some instances for primitive types

  given PrettyString[String] with
    def prettyString(x: String): String = s""""x""""


  given PrettyString[Int] with
    def prettyString(x: Int): String = x.toString
  

  given PrettyString[Long] with
    def prettyString(x: Long): String = x.toString
  

  given PrettyString[Double] with
    def prettyString(x: Double): String = x.toString
  

  given PrettyString[Boolean] with
    def prettyString(x: Boolean): String = x.toString
  

  // our helper method to print the PrettyString
  def prettyPrintln[A](a: A)(using prettyStringInstance: PrettyString[A]) = 
    println(prettyStringInstance.prettyString(a))
}

/************************************** Use it *****************************************/
import PrettyString.prettyPrintln

enum Visitor derives PrettyString { // magic happens here via 'derives'
  case User(name: String, age: Int)
  case AnonymousVisitor
}

import Visitor._

val someVisitors = List(
  User("bob", 25),
  AnonymousVisitor
)

//someVisitors.foreach(prettyPrintln)