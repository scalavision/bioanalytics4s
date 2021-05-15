package doc

// Binary operation, combine two values into a one, new value
// zio.prelude operations
// Associative
// Comutative
// Inverse

object hide {
trait OperatorDescription[A] {
  def binaryOp: A => A
  def unaryOp: (A,A) => A
}

// Can't do much with this:
final case class BoringEvent(description: String)



sealed trait Event:
  self =>
  // associative
  def ++(that: Event): Event =
    Event.Sequential(self, that)

  // associative and commutative
  def &&(that: Event): Event =
    Event.Parallel(self, that)

object Event:
  case object Empty extends Event
  final case class Single(description: String) extends Event
  final case class Parallel(left: Event, right: Event) extends Event
  final case class Sequential(first: Event, second: Event) extends Event

object EventTypes:
  val getOutSugar: Event = ???
  val bakeBread: Event = ???
  val prepareFlour: Event = ???

  prepareFlour ++ bakeBread
  bakeBread ++ prepareFlour

trait Order

case class OrderOperation(f: Order => Order) { self =>
  def andThen(that: OrderOperation): OrderOperation =
    OrderOperation(self.f.andThen(that.f))
}

case class Average(currentSum: Double, currentCount: Long) {
  def <>(that: Average) =
    Average(currentSum + that.currentSum, currentCount + that.currentCount)

  def average = currentSum / currentCount
}


}