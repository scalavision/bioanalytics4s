package doc.ftagless


// https://gist.github.com/mmenestret/0b746cfd650796a639723ee74a3de302
// https://gist.github.com/OlivierBlanvillain/48bb5c66dbb0557da50465809564ee80
// https://www.becompany.ch/en/blog/2020/04/14/tagless-final-best-practices

// https://github.com/SystemFw/Befunge-93
// https://github.com/m50d/paperdoll
// https://github.com/ProjectSeptemberInc/freek

// http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf
// http://okmij.org/ftp/tagless-final/course/lecture.pdf

// https://jproyo.github.io/posts/2019-02-07-practical-tagless-final-in-scala.html

// Tagless final example
trait SqlExprTF[A]:
  def bool(a: Boolean): SqlExpr[Boolean]
  def int(a: Int): SqlExpr[Int]
  def lEq(a: Int, b: Int): Boolean = a <= b
  def and(a: Boolean, b: Boolean): Boolean = a && b
  def or(a: Boolean, b: Boolean): Boolean = a || b

enum SqlExpr[A]:
  case Number(i: Int) extends SqlExpr[Int]
  case Bool(b: Boolean) extends SqlExpr[Boolean]
  case And(a: SqlExpr[Boolean], b: SqlExpr[Boolean]) extends SqlExpr[Boolean]
  case Or(a: SqlExpr[Boolean], b: SqlExpr[Boolean]) extends SqlExpr[Boolean]
  case Not(a: SqlExpr[Boolean]) extends SqlExpr[Boolean]
  case LEq(a: SqlExpr[Int], b: SqlExpr[Int]) extends SqlExpr[Boolean]

object SqlExpr:
  def eval[A](sql: SqlExpr[A]): A = 
    sql match
      case Bool(b) => b
      case Number(i) => i
      case LEq(l,r) => eval(l) <= eval(r)
      case And(l,r) => eval(l) && eval(r)
      case Or(l,r) => eval(l) || eval(r)
      case Not(s) => !eval(s)

object SimpleBool:
  enum SqlExpr:
    case Bool(b: Boolean)
    case And(a: SqlExpr, b: SqlExpr)
    case Or(a: SqlExpr, b: SqlExpr)
    case Not(a: SqlExpr)
    case LEq(a: SqlExpr, b: SqlExpr)

  object SqlExpr:
    def eval(sql: SqlExpr): Boolean = 
      sql match
        case Bool(b) => b
        case LEq(l,r) => eval(l) <= eval(r)
        case And(l,r) => eval(l) && eval(r)
        case Or(l,r) => eval(l) || eval(r)
        case Not(s) => ! eval(s)

