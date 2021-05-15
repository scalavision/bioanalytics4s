package doc.ord

trait Ord[T]:
  def compare(x: T, y: T): Int
  extension(x: T) def < (y: T) = compare(x,y) < 0
  extension(x: T) def > (y: T) = compare(x,y) > 0
  extension(x: T) def <= (y: T) = compare(x,y) <= 0
  extension(x: T) def >= (y: T) = compare(x,y) >= 0

given intOrd: Ord[Int] with
  def compare(x: Int, y: Int): Int =
    if x < y then -1 else if x > y then +1 else 0

given Ord[Double] with
  def compare(x: Double, y: Double): Int =
    if x < y then -1 else if x > y then +1 else 0

given listOrd[T](using ord: Ord[T]): Ord[List[T]] with
  def compare(xs: List[T], ys: List[T]): Int = (xs, ys) match
    case (Nil, Nil) => 0
    case (Nil, _) => -1
    case (_, Nil) => 1
    case (x:: xs1, y :: ys1) =>
      val fst = ord.compare(x,y)
      if fst != 0 then fst else compare(xs1, ys1)

