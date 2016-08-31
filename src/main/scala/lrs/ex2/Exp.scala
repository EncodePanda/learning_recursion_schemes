package lrc.ex2

import matryoshka._, Recursive.ops._
import scalaz._
import Scalaz._

sealed trait Exp[A]

case class IntExp[A](v: Int) extends Exp[A]
case class SumExp[A](exp1: A, exp2: A) extends Exp[A]
case class MultiplyExp[A](exp1: A, exp2: A) extends Exp[A]

object Exp2 extends App {

  val exp0 = SumExp[Exp[Unit]](IntExp(2), IntExp(3))
  val exp1 = MultiplyExp[Exp[Exp[Unit]]](
    SumExp[Exp[Unit]](
      IntExp(2),
      IntExp(2)),
    IntExp[Exp[Unit]](5)
  )

  def evaluate(exp: Exp[Int]): Int = exp match {
    case SumExp(exp1, exp2) => exp1 + exp2
    case MultiplyExp(exp1, exp2) => exp1 * exp2
    case IntExp(v) => v
  }

  println(evaluate(IntExp[Int](10)))
  // will not compile println(evaluate_1(exp0))


}

