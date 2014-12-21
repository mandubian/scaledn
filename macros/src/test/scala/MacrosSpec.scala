/*
 * Copyright (c) 2014 Pascal Voitot
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package scaledn
package macros

import org.scalatest._

import scala.util.{Try, Success, Failure}

import scaledn._
import scaledn.macros._


class MacrosSpec extends FlatSpec with Matchers with TryValues {

  "EDN Macros" should "parse basic types" in {
    val e: String = EDN("\"toto\"")
    e should equal ("toto")

    val bt: Boolean = EDN("true")
    bt should equal (true)

    val bf: Boolean = EDN("false")
    bf should equal (false)

    val l: Long = EDN("123")
    l should equal (123L)

    val d: Double = EDN("123.456")
    d should equal (123.456)

    val bi: BigInt = EDN("123M")
    bi should equal (BigInt("123"))

    val bd: BigDecimal = EDN("123.456N")
    bd should equal (BigDecimal("123.456"))

    val s: EDNSymbol = EDN("foo/bar")
    s should equal (EDNSymbol("foo/bar", Some("foo")))

    val kw: EDNKeyword = EDN(":foo/bar")
    kw should equal (EDNKeyword(EDNSymbol("foo/bar", Some("foo"))))

    val nil: EDNNil.type = EDN("nil")
    nil should equal (EDNNil)

    val list: List[Long] = EDN("(1 2 3)")
    list should equal (List(1L, 2L, 3L))

    val list2: List[Any] = EDN("""(1 "toto" 3)""")
    list2 should equal (List(1L, "toto", 3L))

    val vector: Vector[String] = EDN("""["tata" "toto" "tutu"]""")
    vector should equal (Vector("tata", "toto", "tutu"))

    val vector2: Vector[Any] = EDN("""[1 "toto" 3]""")
    vector2 should equal (Vector(1L, "toto", 3L))

    val set: Set[Double] = EDN("#{1.23 2.45 3.23}")
    set should equal (Set(1.23, 2.45, 3.23))

    val set2: Set[Any] = EDN("""#{1 "toto" 3}""")
    set2 should equal (Set(1L, "toto", 3L))

    val map: Map[Long, String] = EDN("""{ 1 "toto", 2 "tata", 3 "tutu" }""")
    map should equal (Map(1 -> "toto", 2 -> "tata", 3 -> "tutu"))

    val map2: Map[Any, Any] = EDN("""{ 1 "toto", "tata" 2, 3 "tutu" }""")
    map2 should equal (Map(1 -> "toto", "tata" -> 2, 3 -> "tutu"))
  }

  it should "parse seq" in {
    val s: Seq[Any] = EDNs("""(1 2 3) "toto" [true false] :foo/bar""")
    s should equal (Seq(
      Seq(1L, 2L, 3L),
      "toto",
      Vector(true, false),
      EDNKeyword(EDNSymbol("foo/bar", Some("foo")))
    ))
  }

  it should "parse single to hlist" in {
    import shapeless.{HNil, ::}
    import shapeless.record._
    import shapeless.syntax.singleton._

    val s: Long :: String :: Boolean :: HNil = EDNH("""(1 "toto" true)""")
    s should equal (1L :: "toto" :: true :: HNil)

    val s2: Long = EDNH("""1""")
    s2 should equal (1L)

    val s3 = EDNH("""{1 "toto" true 1.234 "foo" (1 2 3)}""")
    s3 should equal (
      1L ->> "toto" ::
      true ->> 1.234 ::
      "foo" ->> List(1L, 2L, 3L) ::
      HNil
    )

    val s4 = EDNHR("""{1 "toto" true 1.234 "foo" (1 2 3)}""")
    s4 should equal (
      1L ->> "toto" ::
      true ->> 1.234 ::
      "foo" ->> (1L :: 2L :: 3L :: HNil) ::
      HNil
    )
  }

  it should "parse multiple to hlist" in {
    import shapeless.{HNil, ::}
    val s: List[Long] :: String :: Vector[Boolean] :: EDNKeyword :: HNil = EDNHs("""(1 2 3) "toto" [true false] :foo/bar""")
    s should equal (
      Seq(1L, 2L, 3L) ::
      "toto" ::
      Vector(true, false) ::
      EDNKeyword(EDNSymbol("foo/bar", Some("foo"))) ::
      HNil
    )

    val s2 = EDNHRs("""(1 2 3) "toto" [true false] :foo/bar""")
    s2 should equal (
      (1L :: 2L :: 3L :: HNil) ::
      "toto" ::
      (true :: false :: HNil) ::
      EDNKeyword(EDNSymbol("foo/bar", Some("foo"))) ::
      HNil
    )
  }

  it should "use string interpolation" in {
    import shapeless.{HNil, ::}

    val l = 123L
    val s = List("foo", "bar")

    val r: Long = EDN(s"$l")

    val r1: Seq[Any] = EDN(s"($l $s)")
    val r2: Long :: List[String] :: HNil = EDNH(s"($l $s)")
  }

}