package doc.expr

import doc.num.*


// Fully generic solution
// Using context function type
enum Expr[+A]:
  self =>
  case Number[A](value: A) extends Expr[A]
  case Add(e1: Expr[A], e2: Expr[A]) extends Expr[A]
  case Mul(e1: Expr[A], e2: Expr[A]) extends Expr[A]
  case Key(value: String) extends Expr[A]

import Expr.*

type Env[A] = Map[String, A]
type WithEnv[A] = Env[A] ?=> A

import EvalHandler.*
def eval[A: Num](exp: Expr[A]): WithEnv[A] = exp match
  case Key(value) => handleKey(value)
  case Number(value) => value
  case Add(l,r) => handleAdd(l,r)
  case Mul(l,r) => handleMul(l,r)

def getEnv[A]: Env[A] ?=> Env[A] = summon[Env[A]]

object EvalHandler:
  import doc.num.ops.*
  def handleKey[A](value: String): WithEnv[A] = getEnv(value)
  def handleAdd[A: Num](l: Expr[A], r: Expr[A]): WithEnv[A]  = eval(l) + eval(r)
  def handleMul[A: Num](l: Expr[A], r: Expr[A]): WithEnv[A]  = eval(l) * eval(r)

def exp1: Expr[Int] = 
  Mul(
    Key("z"),
    Add(
      Number(30),
      Mul(
        Key("x"),
        Key("y")
      )
    )
  )

given someEnv: Env[Int] = Map("x" -> 17, "y" -> 10, "z" -> 2)
def eval1() = eval(exp1)

object calc:
  extension [T](e1: Expr[T])
    def + (e2: Expr[T]) = Add(e1,e2)
    def * (e2: Expr[T]) = Mul(e1,e2)
    
  def ^ (key: String) = Key(key)
  def num [T: Num](v: T) = Number(v)


import calc.*

def exp2: Expr[Int] =
  ^("z") * (^("x") * ^("y") + num(30))

def eval2() = eval(exp2)

object exprWithFunctionContext:
  // Using context function type
  enum Expr:
    self =>
    case Number(value: Int)
    case Add(e1: Expr, e2: Expr)
    case Mul(e1: Expr, e2: Expr)
    case Key(value: String)

  import Expr.*

  type Env = Map[String, Int]
  type WithEnv = Env ?=> Int
  //def eval(exp: Expr)(using env: Env): Int = exp match
  def eval(exp: Expr): WithEnv = exp match
    case Key(value) => handleKey(value)
    case Number(value) => value
    case Add(l,r) => handleAdd(l,r)
    case Mul(l,r) => handleMul(l,r)

  def getEnv: Env ?=> Env = summon[Env]

  def handleKey(value: String): WithEnv     = getEnv.getOrElse(value, 0)
  def handleAdd(l: Expr, r: Expr): WithEnv  = eval(l) + eval(r)
  def handleMul(l: Expr, r: Expr): WithEnv  = eval(l) * eval(r)

  def exp1: Expr = 
    Mul(
      Key("z"),
      Add(
        Number(30),
        Mul(
          Key("x"),
          Key("y")
        )
      )
    )

    //given someEnv: Env = Map("x" -> 17, "y" -> 10, "z" -> 2)
    //def eval1() = eval(exp1)


object originalExprDemo:

  enum Expr:
    case Number(value: Int)
    case Add(e1: Expr, e2: Expr)
    case Mul(e1: Expr, e2: Expr)
    case Key(value: String)

  import Expr.*

  type Env = Map[String, Int]

  def eval(exp: Expr)(using env: Env): Int = exp match
    case Key(value) => handleKey(value)
    case Number(value) => value
    case Add(l,r) => handleAdd(l,r)
    case Mul(l,r) => handleMul(l,r)


  def handleKey(value: String)(using env: Env) = env.getOrElse(value, 0)
  def handleAdd(l: Expr, r: Expr)(using env: Env) = eval(l) + eval(r)
  def handleMul(l: Expr, r: Expr)(using env: Env) = eval(l) * eval(r)

  val exp1: Expr = 
    Mul(
      Key("z"),
      Add(
        Number(30),
        Mul(
          Key("x"),
          Key("y")
        )
      )
    )

  given env: Env = Map("x" -> 17, "y" -> 10, "z" -> 2)
  val eval1 = eval(exp1)
