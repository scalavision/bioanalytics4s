package vcf

import scala.language.postfixOps
// extension [A, B](m: Map[A, B])
//   def *> (f: VcfTransformer[A] => VcfTransformer[B]): VcfTransformer[B] =
//     VcfTransformer.FlatMap[A, B](self, f)

// extension [B <: VcfTransformer.Columns](fn: String => vcf.VcfTransformer.Map[String, String])
//   def *> (s: String)(f: String => B): B =
//     VcfTransformer.FlatMap[String, B](s, f)

enum VcfTransformer [+A] { self =>
  case Init(f: String => String => VcfTransformer[String]) extends VcfTransformer[String]
  case Map[A, B](a: A, f: A => VcfTransformer[B]) extends VcfTransformer[B]
  case FlatMap[A, B](value: VcfTransformer[A], f: VcfTransformer[A] => VcfTransformer[B]) extends VcfTransformer[B]
  case MapWithFilter[A, B](value: VcfTransformer[A], predicate: VcfFilter[A], f: A => VcfTransformer[B]) extends VcfTransformer[B]
  case Line(value: String) extends VcfTransformer[String]
  case Columns(values: IndexedSeq[String]) extends VcfTransformer[IndexedSeq[String]]

  // @scala.annotation.targetName("lift1")
  // def *> [B](f: A => VcfTransformer[B]): VcfTransformer[B] =
  //   FlatMap[A, B](self, f)

  @scala.annotation.targetName("lift2")
  def *> [A2 >: A, B](f: VcfTransformer[A2] => VcfTransformer[B]): VcfTransformer[B] =
    FlatMap[A2, B](self, f)

  def |> [A2 >: A, B](predicate: VcfFilter[A2], f: A2 => VcfTransformer[B]): VcfTransformer[B] =
    MapWithFilter(self, predicate, f)
}

object VcfTransformer:
  import VcfTransformer._

  def columns(splitter: Char = '\t'): VcfTransformer[String] => Columns = {
    case Line(value) => 
      Columns(value.split('\t').toIndexedSeq)
    case _ => throw new Exception("inconcivable")
  }

  def transformColumn[A, B](index: Int, f: A => B): Line => Columns = ???
    
  def splitOnTab =
    columns()


  def init = Init.apply((s: String) => Line.apply)

  def vcfLineToColumns =
    init *> splitOnTab
    

  
  // def run[A](line: String, t: VcfTransformer[A]): B = t match
  //   case Map(value, f) => f(run(value))
  //   case Line(s) => s
  //   case Columns(seq) => seq
    
