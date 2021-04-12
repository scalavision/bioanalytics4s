package vcf

import scala.deriving.Mirror
import scala.compiletime.constValue
import scala.compiletime.erasedValue
import scala.compiletime.summonInline

object auto:

  inline def getTypeclassInstances[F[_], A <: Tuple]: List[F[Any]] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (head *: tail) =>
        val headTypeClass = summonInline[F[head]]
        val tailTypeClasses = getTypeclassInstances[F, tail]
        headTypeClass.asInstanceOf[F[Any]] :: tailTypeClasses

  inline def summonInstanceHelper[F[_], A](using m: Mirror.Of[A]) =
    getTypeclassInstances[F, m.MirroredElemTypes]

  inline def getElemLabels[A <: Tuple]: List[String] =
    inline erasedValue[A] match
      case _: EmptyTuple => Nil
      case _: (head *: tail) =>
        val headElementLabel = constValue[head].toString()
        val tailElementLabels = getElemLabels[tail]
        headElementLabel :: tailElementLabels
  
  inline def createTupleFromProduct[A](using m: Mirror.Of[A]) =
    getElemLabels[m.MirroredElemLabels]

  inline def labelFromMirror[A](using m: Mirror.Of[A]): String =
    constValue[m.MirroredLabel]
