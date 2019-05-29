package sandbox.chapter04

import cats.data.State
import cats.syntax.applicative._

object Calculator {
  type CalcState[A] = State[List[Int], A]

  /*
    State[List[Int], Int] { oldStack =>
      val newStack = someTransformation(oldStack)
      val result = someCalculation
      (newStack, result)
    }
  */
  def evalOne(sym: String): CalcState[Int] = {
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }
  }

  private def operand(num: Int): CalcState[Int] =
    State[List[Int], Int] { stack =>
      (num :: stack, num)
    }

  private def operator(func: (Int, Int) => Int): CalcState[Int] =
    State[List[Int], Int] {
      case x :: y :: tail =>
        val calculation = func(x, y)
        (calculation :: tail, calculation)
      case _ => sys.error("Error!")
  }

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]) { (state, sym) =>
      state.flatMap(_ => evalOne(sym))
    }
  }

  def evalInput(input: String): Int = {
    evalAll(input.split(" ").toList).runA(Nil).value
  }

}
