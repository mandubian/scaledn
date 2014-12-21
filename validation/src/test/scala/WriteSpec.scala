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
package scaledn.validation

import org.scalatest._

import scala.util.{Try, Success, Failure}

import play.api.data.mapping._
import play.api.libs.functional.syntax._

import scaledn._
import scaledn.write._


class WriteSpec extends FlatSpec with Matchers with TryValues {

  case class Address(street: String, cp: Int)
  case class Person(name: String, age: Int, addr: Address)

  "EDN Write" should "write basic types" in {
    toEDNString("toto") should equal ("\"toto\"")
    toEDNString(true) should equal ("true")
    toEDNString(false) should equal ("false")
    toEDNString(123L) should equal ("123")
    toEDNString(123) should equal ("123")
    toEDNString(BigInt("123")) should equal ("123N")
    toEDNString(123.456) should equal ("123.456")
    toEDNString(BigDecimal("123.456")) should equal ("123.456M")
    toEDNString('c') should equal ("""\c""")
    toEDNString('\r') should equal ("""\return""")
    toEDNString('\n') should equal ("""\newline""")
    toEDNString('\u0308') should equal ("\\u0308")
  }

  it should "write collections" in {
    toEDNString(List(1, 2, 3)) should equal ("""(1 2 3)""")
    toEDNString(Vector(1, 2, 3)) should equal ("""[1 2 3]""")
    toEDNString(Set(1, 2, 3)) should equal ("""#{1 2 3}""")
    toEDNString(Map("toto" -> 1, "tata" -> 2, "tutu" -> 3)) should equal ("""{"toto" 1,"tata" 2,"tutu" 3}""")
    toEDNString(Seq(1, 2, 3)) should equal ("""(1 2 3)""")
  }

  it should "write to path" in {
    toEDNString((Path \ "foo" \ "bar").write[String, EDNMap].writes("toto")) should equal ("""{"foo" {"bar" "toto"}}""")
  }

  it should "write hlist" in {
    import shapeless.{::, HNil}
    // import Writes._

    implicitly[Write[Long :: String :: HNil, String]]

    toEDNString(1 :: true :: List(1L, 2L, 3L) :: HNil) should equal ("""(1 true (1 2 3))""")
  }

  it should "write case class & tuple" in {
    import shapeless.{::, HNil}

    toEDNString(Person("toto", 34, Address("chboing", 75009))) should equal (
      """{"name" "toto", "age" 34, "addr" {"street" "chboing", "cp" 75009}}"""
    )

    toEDNString((23, true)) should equal ("""(23 true)""")
    toEDNString((23, Vector(1, 2, 3), "toto" :: 2 :: true :: HNil, Person("toto", 34, Address("chboing", 75009)))) should equal (
      """(23 [1 2 3] ("toto" 2 true) {"name" "toto", "age" 34, "addr" {"street" "chboing", "cp" 75009}})"""
    )
  }

}
