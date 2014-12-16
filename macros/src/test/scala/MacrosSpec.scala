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

    val l: Long = edn1("123")
    l should equal (123L)

    val d: Double = edn1("123.456")
    d should equal (123.456)

    val bi: BigInt = edn1("123M")
    bi should equal (BigInt("123"))

    val bd: BigDecimal = edn1("123.456N")
    bd should equal (BigDecimal("123.456"))

    val s: EDNSymbol = edn1("foo/bar")
    s should equal (EDNSymbol("foo/bar", Some("foo")))

    val kw: EDNKeyword = edn1(":foo/bar")
    kw should equal (EDNKeyword(EDNSymbol("foo/bar", Some("foo"))))
  }

}