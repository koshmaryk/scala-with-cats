package sandbox.chapter04

import cats.data.Reader
import cats.syntax.applicative._

object LoginSystem {

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] = {
    Reader(db => db.usernames.get(userId))
  }

  def checkPassword(
                     username: String,
                     password: String): DbReader[Boolean] = {

    Reader(db => db.passwords.get(username).contains(password))
  }

  def checkLogin(
                   userId: Int,
                   password: String): DbReader[Boolean] = {

    for {
      username <- findUsername(userId)
      validPassword <- username.map(checkPassword(_, password)).getOrElse(false.pure[DbReader])
    } yield validPassword
  }

}
