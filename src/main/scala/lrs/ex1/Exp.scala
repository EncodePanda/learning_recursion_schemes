package lrc.ex1

sealed trait Exp

case class IntExp(v: Int) extends Exp
case class SumExp(exp1: Exp, exp2: Exp) extends Exp
case class MultiplyExp(exp1: Exp, exp2: Exp) extends Exp

object Exp1 extends App {

  def evaluate(exp: Exp): Int = exp match {
    case SumExp(exp1, exp2) => evaluate(exp1) + evaluate(exp2)
    case MultiplyExp(exp1, exp2) => evaluate(exp1) *  evaluate(exp2)
    case IntExp(v) => v
  }

  val exp0 = SumExp(IntExp(2), IntExp(3))
  val exp1 = MultiplyExp(SumExp(IntExp(2), IntExp(2)), IntExp(5))
  val exp2 = SumExp(MultiplyExp(IntExp(2), IntExp(2)), IntExp(5))

  println(evaluate(exp1))
  println(evaluate(exp2))

}

