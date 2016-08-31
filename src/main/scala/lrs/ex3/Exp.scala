package lrc.ex3

import matryoshka._, Recursive.ops._
import scalaz._
import Scalaz._

sealed trait Exp[A]

case class IntExp[A](v: Int) extends Exp[A]
case class SumExp[A](exp1: A, exp2: A) extends Exp[A]
case class MultiplyExp[A](exp1: A, exp2: A) extends Exp[A]

object Exp3 extends App {

  implicit object ExpFunctor extends Functor[Exp] {
    def map[A, B](fa: Exp[A])(f: A => B): Exp[B] = fa match {
      case IntExp(v) => IntExp[B](v)
      case SumExp(e1, e2) => SumExp(f(e1), f(e2))
      case MultiplyExp(e1, e2) => MultiplyExp(f(e1), f(e2))
    }
  }
  val exp0 = SumExp[Fix[Exp]](Fix(IntExp(2)), Fix(IntExp(3)))

  val exp1 = MultiplyExp[Fix[Exp]](
    Fix(SumExp[Fix[Exp]](
      Fix(IntExp(2)),
      Fix(IntExp(2)))),
    Fix(IntExp(5))
  )

  val exp2 = SumExp[Fix[Exp]](
    Fix(MultiplyExp[Fix[Exp]](
      Fix(IntExp(2)),
      Fix(IntExp(2)))),
    Fix(IntExp(5))
  )


  def calc: Algebra[Exp, Int] = (exp: Exp[Int]) => exp match {
    case SumExp(exp1, exp2) => exp1 + exp2
    case MultiplyExp(exp1, exp2) => exp1 * exp2
    case IntExp(v) => v
  }

  def evaluate(exp: Fix[Exp]): Int = exp.cata(calc)

  println(evaluate(Fix(exp0)))
  println(evaluate(Fix(exp1)))
  println(evaluate(Fix(exp2)))


}

