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
package validate

import org.scalatest._

import scala.util.{Try, Success, Failure}

import play.api.data.mapping._
import play.api.data.mapping.{Success => VS}

import scaledn._
import scaledn.parser._
import scaledn.validate._


class ValidateSpec extends FlatSpec with Matchers with TryValues {

  case class CP(cp: Int)
  case class Address(street: String, cp: CP)
  case class Person(name: String, age: Int, addr: Address)

  "EDN Validation" should "validate String" in {
    parseEDN("\"foobar\"").map(validateEDN[String]).success.value should be (
      play.api.data.mapping.Success("foobar")
    )

    parseEDN("\"foobar\"").map(Path.read[EDN, String].validate).success.value should be (
      play.api.data.mapping.Success("foobar")
    )

    parseEDN("\"foobar\"").map(From[EDN]{ __ => __.read[String] }.validate).success.value should be (
      play.api.data.mapping.Success("foobar")
    )
  }

  it should "validate Long" in {
    parseEDN("12345").map(validateEDN[Long]).success.value should be (
      play.api.data.mapping.Success(12345L)
    )
  }

  it should "validate Double" in {
    parseEDN("12345.123").map(validateEDN[Double]).success.value should be (
      play.api.data.mapping.Success(12345.123)
    )
  }

  it should "validate Symbol" in {
    parseEDN("foo.foo2/bar").map(validateEDN[EDNSymbol]).success.value should be (
      play.api.data.mapping.Success(EDNSymbol("foo.foo2" / "bar"))
    )
  }

  it should "validate Keyword" in {
    parseEDN(":foo.foo2/bar").map(validateEDN[EDNKeyword]).success.value should be (
      play.api.data.mapping.Success(EDNKeyword("foo.foo2" / "bar"))
    )
  }

  it should "validate List[Long]" in {
    parseEDN("""(1 2 3 4 5)""").map(validateEDN[List[Long]]).success.value should be (
      play.api.data.mapping.Success(List(1L, 2L, 3L, 4L, 5L))
    )

    parseEDN("""(1 2 3 "toto" 5)""").map(validateEDN[List[Long]]).success.value should be (
      play.api.data.mapping.Failure(Seq(Path \ 3 -> Seq(ValidationError("error.number", "Long"))))
    )
  }

  it should "validate Seq[Long]" in {
    parseEDN("""(1 2 3 4 5)""").map(validateEDN[Seq[Long]]).success.value should be (
      play.api.data.mapping.Success(List(1L, 2L, 3L, 4L, 5L))
    )
  }

  it should "validate Vector[Long]" in {
    parseEDN("""[1 2 3 4 5]""").map(validateEDN[Vector[Long]]).success.value should be (
      play.api.data.mapping.Success(Vector(1L, 2L, 3L, 4L, 5L))
    )
    parseEDN("""[1 2 3 "toto" 5]""").map(validateEDN[Vector[Long]]).success.value should be (
      play.api.data.mapping.Failure(Seq(Path \ 3 -> Seq(ValidationError("error.number", "Long"))))
    )
  }

  it should "validate Set[Long]" in {
    parseEDN("""#{1 2 3 4 5}""").map(validateEDN[Set[Long]]).success.value should be (
      play.api.data.mapping.Success(Set(1L, 2L, 3L, 4L, 5L))
    )
    parseEDN("""#{1 2 3 "toto" 5}""").map(validateEDN[Set[Long]]).success.value should be (
      play.api.data.mapping.Failure(Seq(Path \ "toto" -> Seq(ValidationError("error.number", "Long"))))
    )
  }

  it should "validate Map[String, Long]" in {
    parseEDN("""{ "toto" 1 "tata" 2 "tutu" 3 }""").map(validateEDN[Map[String, Long]]).success.value should be (
      play.api.data.mapping.Success(Map(
        "toto" -> 1L,
        "tata" -> 2L,
        "tutu" -> 3L
      ))
    )
  }

  it should "validate Map[Long, String]" in {
    parseEDN("""{ 1 "toto" 2 "tata" 3 "tutu" }""").map(validateEDN[Map[Long, String]]).success.value should be (
      play.api.data.mapping.Success(Map(
        1L -> "toto",
        2L -> "tata",
        3L -> "tutu"
      ))
    )
  }

  it should "validate Option[Long]" in {
    parseEDN("""{ "toto" 1 "tata" 2 "tutu" 3 }""").map(
      (Path \ "tata").read[EDN, Option[Long]].validate
    ).success.value should be (
      play.api.data.mapping.Success(Some(2))
    )
  }

  it should "validate deep path Double" in {
    parseEDN("""{ "toto" 1, "tata" { "foo" 2.123, "bar" 3 }, "tutu" 3 }""").map(
      (Path \ "tata" \ "foo").read[EDN, Double].validate
    ).success.value should be (
      play.api.data.mapping.Success(2.123)
    )
  }

  it should "validate hlist" in {
    import shapeless._
    import scaledn.EDNNil

    parseEDN("""(1 "toto" true nil)""").map(
      validateEDN[Long :: String :: Boolean :: EDNNil.type :: HNil]
    ).success.value should be (
      play.api.data.mapping.Success(1L :: "toto" :: true :: EDNNil :: HNil)
    )

    parseEDN("""[1 "toto" true nil]""").map(
      validateEDN[Long :: String :: Boolean :: EDNNil.type :: HNil]
    ).success.value should be (
      play.api.data.mapping.Success(1L :: "toto" :: true :: EDNNil :: HNil)
    )
  }

  it should "validate case class / tuples" in {
    import shapeless.HasProductGeneric

    parseEDN("""("toto" 34 ("chboing" (75009)))""").map(
      validateEDN[Person]
    ).success.value should be (
      play.api.data.mapping.Success(Person("toto", 34, Address("chboing", CP(75009))))
    )

    parseEDN("""{"name" "toto", "age" 34, "addr" {"street" "chboing", "cp" {"cp" 75009}}}""").map(
      validateEDN[Person]
    ).success.value should be (
      play.api.data.mapping.Success(Person("toto", 34, Address("chboing", CP(75009))))
    )

    parseEDN("""("toto" 34 {"street" "chboing", "cp" {"cp" 75009}})""").map(
      validateEDN[Tuple3[String, Int, Address]]
    ).success.value should be (
      play.api.data.mapping.Success(("toto", 34, Address("chboing", CP(75009))))
    )

  }
}