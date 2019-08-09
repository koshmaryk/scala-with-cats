package sandbox.chapter05

import cats.data.EitherT
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

object Transformer {

  type Response[A] = EitherT[Future, String, A]

  def getPowerLevel(autobot: String): Response[Int] =
    powerLevels.get(autobot) match {
      case Some(powerLevel) => EitherT.right(Future(powerLevel))
      case None => EitherT.left(Future(s"$autobot is unreachable."))
    }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] =
    for {
      powerLevel1 <- getPowerLevel(ally1)
      powerLevel2 <- getPowerLevel(ally2)
    } yield (powerLevel1 + powerLevel2) > 15

  def tacticalReport(ally1: String, ally2: String): String = {
    val value = canSpecialMove(ally1, ally2).value

    Await.result(value, 1.second) match {
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
      case Left(message) =>
        s"Comms error: $message"
    }
  }

  private val powerLevels = Map(
    "Jazz"      -> 6,
    "Bumblebee" -> 8,
    "Hot Rod"   -> 10
  )
}
