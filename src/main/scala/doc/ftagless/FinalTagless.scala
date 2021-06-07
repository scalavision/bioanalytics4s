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

trait SqlExpr[A]:
  def value(a: A): A  

/* This without GADT will be lagorous!
enum SqlExpr:
  case Number(i: Int)
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
*/

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

