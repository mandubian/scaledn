package scaledn
package macros

import org.scalatest._

import scala.util.{Try, Success, Failure}

import scaledn._
import scaledn.macros._

class MacrosSpec extends FlatSpec with Matchers with TryValues {

  "EDN Macros" should "parse basic types" in {
    val e: String = edn1("\"toto\"")
    e should equal ("toto")

    val bt: Boolean = edn1("true")
    bt should equal (true)

    val bf: Boolean = edn1("false")
    bf should equal (false)
  }

}