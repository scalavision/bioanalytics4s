package doc.num

trait Num[T]:
  def add(t1: T, t2: T): T
  def mul(t1: T, t2: T): T
  extension (t: T) def square: T = mul(t,t)

object Num:

  // removes the need for summon[Num[T]], can use Num[T].add directly
  def apply[T](using instance: Num[T]): Num[T] = instance

  def to[T](addN: (T,T) => T, mulN: (T,T) => T): Num[T] = new Num {
      def add(t1: T, t2: T): T = addN(t1,t2)
      def mul(t1: T, t2: T): T = mulN(t1,t2)
  }

  given Num[Int] = to( (s1, s2) => s1 + s2 , (t1,t2) => t1 * t2)
  given Num[Double] = to( (s1, s2) => s1 + s2 , (t1,t2) => t1 * t2)
  //given Num[Short] = to[Short]( (s1, s2) => s1 + s2 , (t1,t2) => t1 * t2)
  given Num[Float] = to( (s1, s2) => s1 + s2 , (t1,t2) => t1 * t2)
  given Num[Long] = to( (s1, s2) => s1 + s2 , (t1,t2) => t1 * t2)

  // Just an example, should not use zip here for real
  given [T] (using Num[T]): Num[List[T]] = to[List[T]](
    (l1,l2) => l1.zip(l2).map { (n1,n2) => Num[T].add(n1, n2) },
    (l1,l2) => l1.zip(l2).map { (n1,n2) => Num[T].mul(n1, n2) }
  )

object ops:

  extension [T](t1: T)(using n: Num[T])
    def + (t2: T): T = n.add(t1,t2)
    def * (t2: T): T = n.mul(t1,t2)

  def sumList[T](ts: List[T])(using n: Num[T]): T =
    ts.reduce((a,b) => a + b )

object TestNum:
  import Num.*
  import ops.*

  val l1 = List(1,2,3,4)
  val result1 = sumList(l1)

