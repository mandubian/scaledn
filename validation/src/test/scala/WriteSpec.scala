package scaledn.validation

import org.scalatest._

import scala.util.{Try, Success, Failure}

import scaledn._
import scaledn.write._
import play.api.data.mapping._
import play.api.libs.functional.syntax._

class WriteSpec extends FlatSpec with Matchers with TryValues {

  case class Person(name: String, age: Int)
  object Person {
    // implicit val personRule = {
    //   import validate.Rules._
    //   Rule.gen[EDNMap, Person]
    // }
    // implicit val personWrite = {
    //   import write.Writes._
    //   Write.gen[Person, EDNMap]
    // }
  }

  "EDN Write" should "write basic types" in {
    toEDNString("toto") should equal ("\"toto\"")
    toEDNString(true) should equal ("true")
    toEDNString(false) should equal ("false")
    toEDNString(123L) should equal ("123")
    toEDNString(123) should equal ("123")
    toEDNString(BigInt("123")) should equal ("123M")
    toEDNString(123.456) should equal ("123.456")
    toEDNString(BigDecimal("123.456")) should equal ("123.456N")
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
    import Writes._
    
    implicitly[Write[Long :: String :: HNil, String]]

    toEDNString(1 :: true :: List(1L, 2L, 3L) :: HNil) should equal ("""(1 true (1 2 3))""")
  }

  it should "write case class" in {
    import shapeless._
    implicitly[Generic.Aux[Person, String :: Int :: HNil]]

    // fieldType['name.narrow, String]
    // val p = LabelledGeneric[Person]

    // val t: Int = p.to(Person("toto", 34))
    // println(toEDNString(Person("toto", 34)))
    //println(Person.personWrite.writes(Person("toto", 34)))
  }
}
