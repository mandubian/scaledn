package scaledn
package macros

import org.scalatest._

import scala.util.{Try, Success, Failure}

import scaledn._
import scaledn.macros._

class MacrosSpec extends FlatSpec with Matchers with TryValues {

  "EDN Macros" should "parse basic types" in {
    val e: String = edn("\"toto\"")
    e should equal ("toto")

    val bt: Boolean = edn("true")
    bt should equal (true)

    val bf: Boolean = edn("false")
    bf should equal (false)

    val l: Long = edn("123")
    l should equal (123L)

    val d: Double = edn("123.456")
    d should equal (123.456)

    val bi: BigInt = edn("123M")
    bi should equal (BigInt("123"))

    val bd: BigDecimal = edn("123.456N")
    bd should equal (BigDecimal("123.456"))

    val s: EDNSymbol = edn("foo/bar")
    s should equal (EDNSymbol("foo/bar", Some("foo")))

    val kw: EDNKeyword = edn(":foo/bar")
    kw should equal (EDNKeyword(EDNSymbol("foo/bar", Some("foo"))))

    val nil: EDNNil.type = edn("nil")
    nil should equal (EDNNil)

    val list: List[Long] = edn("(1 2 3)")
    list should equal (List(1L, 2L, 3L))

    val list2: List[Any] = edn("""(1 "toto" 3)""")
    list2 should equal (List(1L, "toto", 3L))

    val vector: Vector[String] = edn("""["tata" "toto" "tutu"]""")
    vector should equal (Vector("tata", "toto", "tutu"))

    val vector2: Vector[Any] = edn("""[1 "toto" 3]""")
    vector2 should equal (Vector(1L, "toto", 3L))

    val set: Set[Double] = edn("#{1.23 2.45 3.23}")
    set should equal (Set(1.23, 2.45, 3.23))

    val set2: Set[Any] = edn("""#{1 "toto" 3}""")
    set2 should equal (Set(1L, "toto", 3L))

    val map: Map[Long, String] = edn("""{ 1 "toto", 2 "tata", 3 "tutu" }""")
    map should equal (Map(1 -> "toto", 2 -> "tata", 3 -> "tutu"))

    val map2: Map[Any, Any] = edn("""{ 1 "toto", "tata" 2, 3 "tutu" }""")
    map2 should equal (Map(1 -> "toto", "tata" -> 2, 3 -> "tutu"))
  }

  "EDN Macros" should "parse seq" in {
    val s: Seq[Any] = edns("""(1 2 3) "toto" [true false] :foo/bar""")
    s should equal (Seq(
      Seq(1L, 2L, 3L),
      "toto",
      Vector(true, false),
      EDNKeyword(EDNSymbol("foo/bar", Some("foo")))
    ))
  }

}