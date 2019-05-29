package sandbox.chapter04

import org.scalatest.{FlatSpec, Matchers}
import sandbox.chapter04.LoginSystem.Db
import sandbox.chapter04.LoginSystem._

class LoginSystemSpec extends FlatSpec with Matchers {

  private val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  private val passwords = Map(
    "dade"  -> "zerocool",
    "kate"  -> "acidburn",
    "margo" -> "secret"
  )

  private val db = Db(users, passwords)

  behavior of "LoginSystem"

  it should "return true for valid userId and correct password" in {
    checkLogin(1, "zerocool").run(db) shouldBe true
  }

  it should "return false for valid userId and incorrect password" in {
    checkLogin(1, "password").run(db) shouldBe false
  }

  it should "return false for invalid userId" in {
    checkLogin(4, "davinci").run(db) shouldBe false
  }
}
