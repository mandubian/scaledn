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

/**
  * Write Rules to serialize Scala/Shapeless structures to EDN formatted String
  *
  * {{{
  * import scaledn._
  * import write._
  * 
  * // All primitives types are managed
  * toEDNString("toto") should equal ("\"toto\"")
  * toEDNString(123) should equal ("123")
  * toEDNString(BigInt("123")) should equal ("123M")
  * toEDNString(123.456) should equal ("123.456")
  * toEDNString(BigDecimal("123.456")) should equal ("123.456N")
  * toEDNString('\r') should equal ("""\return""")
  * toEDNString('\u0308') should equal ("\\u0308")
  *
  * // Scala collections too
  * toEDNString(List(1, 2, 3)) should equal ("""(1 2 3)""")
  * toEDNString(Vector(1, 2, 3)) should equal ("""[1 2 3]""")
  * toEDNString(Set(1, 2, 3)) should equal ("""#{1 2 3}""")
  * toEDNString(Map("toto" -> 1, "tata" -> 2, "tutu" -> 3)) should equal ("""{"toto" 1,"tata" 2,"tutu" 3}""")
  *
  * // Shapeless HList
  * toEDNString(1 :: true :: List(1L, 2L, 3L) :: HNil) should equal ("""(1 true (1 2 3))""")
  *
  * // Tuples
  * toEDNString(
  *   (23, Vector(1, 2, 3), "toto" :: 2 :: true :: HNil, Person("toto", 34, Address("chboing", 75009)))
  * ) should equal (
  *   """(23 [1 2 3] ("toto" 2 true) {"name" "toto", "age" 34, "addr" {"street" "chboing", "cp" 75009}})"""
  * )
  *
  * // Case classes (without)
  * case class Address(street: String, cp: Int)
  * case class Person(name: String, age: Int, addr: Address)
  *
  * toEDNString(Person("toto", 34, Address("chboing", 75009))) should equal (
  *   """{"name" "toto", "age" 34, "addr" {"street" "chboing", "cp" 75009}}"""
  * )
  * }}}
  */
package object write extends Writes {
  import  play.api.data.mapping.WriteLike

  /**
    * Serializes Scala/Shapeless structures to EDN formatted String
    *
    * {{{
    * import scaledn._
    * import write._
    * 
    * // All primitives types are managed
    * toEDNString("toto") should equal ("\"toto\"")
    * }}}
    */
  def toEDNString[I, O](i: I)(implicit s: EDNStrategy[I, O], w: WriteLike[O, String]): String = w.writes(i)

  /**
    * Serializes Scala/Shapeless structures to a EDN Tagged value
    *
    * {{{
    * import scaledn._
    * import write._
    * 
    * implicit val taggedAddr = Write{ addr: Address =>
    *   EDN(s"#myns/addr {:street ${addr.street}, :cp ${addr.cp}}")
    * }
    * 
    * tagged(Addr("Paris", CP(75009))) should equal (EDN("#myns/addr {:street "Paris", :cp 75009}"))
    * }}}
    */
  def tagged[I, J](i: I)(implicit w: WriteLike[I, EDNTagged[J]]): EDNTagged[J] = w.writes(i)
}

/**
  * Validation Rules to validate EDN types to Scala/Shapeless structures
  *
  * As explained in common package doc, all primitive EDN types are isomorphic/bijective
  * to Scala types so the validation is often a simple runtime cast except for 
  * heterogenous collection where using HList becomes really interesting.
  *
  * Validation is also interesting for case classes.
  *
  * It is based on generic [validation API](https://github.com/jto/validation) developed 
  * by Julien Tournay.
  *
  * {{{
  * import scaledn._
  * import parser._
  * import validate._
  *
  * // basic types
  * parseEDN("\"foobar\"").map(validateEDN[String]).success.value should be (
  *   play.api.data.mapping.Success("foobar")
  * )
  * parseEDN("12345").map(validateEDN[Long]).success.value should be (
  *   play.api.data.mapping.Success(12345L)
  * )
  * parseEDN("12345.123").map(validateEDN[Double]).success.value should be (
  *   play.api.data.mapping.Success(12345.123)
  * )
  *
  * // Scala collections
  * parseEDN("""(1 2 3 4 5)""").map(validateEDN[List[Long]]).success.value should be (
  *   play.api.data.mapping.Success(List(1L, 2L, 3L, 4L, 5L))
  * )
  * parseEDN("""(1 2 3 "toto" 5)""").map(validateEDN[List[Long]]).success.value should be (
  *   play.api.data.mapping.Failure(Seq(Path \ 3 -> Seq(ValidationError("error.number", "Long"))))
  * )
  * parseEDN("""[1 2 3 4 5]""").map(validateEDN[Vector[Long]]).success.value should be (
  *   play.api.data.mapping.Success(Vector(1L, 2L, 3L, 4L, 5L))
  * )
  * parseEDN("""#{1 2 3 4 5}""").map(validateEDN[Set[Long]]).success.value should be (
  *   play.api.data.mapping.Success(Set(1L, 2L, 3L, 4L, 5L))
  * )
  * parseEDN("""{ 1 "toto" 2 "tata" 3 "tutu" }""").map(validateEDN[Map[Long, String]]).success.value should be (
  *   play.api.data.mapping.Success(Map(
  *     1L -> "toto",
  *     2L -> "tata",
  *     3L -> "tutu"
  *   ))
  * )
  *
  * // Shapeless heterogenous lists
  * parseEDN("""(1 "toto" true nil)""").map(
  *   validateEDN[Long :: String :: Boolean :: EDNNil.type :: HNil]
  * ).success.value should be (
  *   play.api.data.mapping.Success(1L :: "toto" :: true :: EDNNil :: HNil)
  * )
  *
  * // Path validation in a map only
  * parseEDN("""{ "toto" 1, "tata" { "foo" 2.123, "bar" 3 }, "tutu" 3 }""").map(
  *   (Path \ "tata" \ "foo").read[EDN, Double].validate
  * ).success.value should be (
  *   play.api.data.mapping.Success(2.123)
  * )
  *
  * // case class validation from heteregenous collections
  * parseEDN("""("toto" 34 ("chboing" (75009)))""").map(
  *   validateEDN[Person]
  * ).success.value should be (
  *   play.api.data.mapping.Success(Person("toto", 34, Address("chboing", CP(75009))))
  * )
  *
  * // case class validation from heteregenous maps
  * parseEDN("""{"name" "toto", "age" 34, "addr" {"street" "chboing", "cp" {"cp" 75009}}}""").map(
  *   validateEDN[Person]
  * ).success.value should be (
  *   play.api.data.mapping.Success(Person("toto", 34, Address("chboing", CP(75009))))
  * )
  *
  * // tuple validation from heteregenous maps
  * parseEDN("""("toto" 34 {"street" "chboing", "cp" {"cp" 75009}})""").map(
  *   validateEDN[Tuple3[String, Int, Address]]
  * ).success.value should be (
  *   play.api.data.mapping.Success(("toto", 34, Address("chboing", CP(75009))))
  * )
  *
  * }}}
  * 
  */
package object validate extends Rules {
  import  play.api.data.mapping.{RuleLike, Validation}

  /**
    * Validate an EDN type to a Scala type
    *
    * It is based on generic [[https://github.com/jto/validation Validation API]] developed 
    * by Julien Tournay.
    *
    * {{{
    * import scaledn._
    * import validate._
    *
    * // validation
    * // Path validation in a map only
    * parseEDN("""{ "toto" 1, "tata" { "foo" 2.123, "bar" 3 }, "tutu" 3 }""").map(
    *   (Path \ "tata" \ "foo").read[EDN, Double].validate
    * ).success.value should be (
    *   play.api.data.mapping.Success(2.123)
    * )
    *
    * // case class validation from heteregenous collections
    * parseEDN("""("toto" 34 ("chboing" (75009)))""").map(
    *   validateEDN[Person]
    * ).success.value should be (
    *   play.api.data.mapping.Success(Person("toto", 34, Address("chboing", CP(75009))))
    * )
    *
    * // case class validation from heteregenous maps
    * parseEDN("""{"name" "toto", "age" 34, "addr" {"street" "chboing", "cp" {"cp" 75009}}}""").map(
    *   validateEDN[Person]
    * ).success.value should be (
    *   play.api.data.mapping.Success(Person("toto", 34, Address("chboing", CP(75009))))
    * )
    *
    * // tuple validation from heteregenous maps
    * parseEDN("""("toto" 34 {"street" "chboing", "cp" {"cp" 75009}})""").map(
    *   validateEDN[Tuple3[String, Int, Address]]
    * ).success.value should be (
    *   play.api.data.mapping.Success(("toto", 34, Address("chboing", CP(75009))))
    * )
    * }}}
    */
  def validateEDN[T](edn: EDN)(implicit r: RuleLike[EDN, T]): Validation[EDN, T] = r.validate(edn)
}