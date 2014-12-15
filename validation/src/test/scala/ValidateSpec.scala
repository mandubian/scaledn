package scaledn
package validate

import org.scalatest._

import scala.util.{Try, Success, Failure}

import scaledn._
import scaledn.parser._
import scaledn.validate._
import play.api.data.mapping._

import play.api.data.mapping.{Success => VS}

class ValidateSpec extends FlatSpec with Matchers with TryValues {

  import scaledn.validate.Rules._

  def validate[T](edn: EDN)(implicit r: Rule[EDN, T]): Validation[EDN, T] = r.validate(edn)

  "EDN Validation" should "validate String" in {
    parseEDNFirst("\"foobar\"").map(validate[String]).success.value should be (
      play.api.data.mapping.Success("foobar")
    )

    parseEDNFirst("\"foobar\"").map(Path.read[EDN, String].validate).success.value should be (
      play.api.data.mapping.Success("foobar")
    )

    parseEDNFirst("\"foobar\"").map(From[EDN]{ __ => __.read[String] }.validate).success.value should be (
      play.api.data.mapping.Success("foobar")
    )
  }

  it should "validate Long" in {
    parseEDNFirst("12345").map(validate[Long]).success.value should be (
      play.api.data.mapping.Success(12345L)
    )
  }

  it should "validate Double" in {
    parseEDNFirst("12345.123").map(validate[Double]).success.value should be (
      play.api.data.mapping.Success(12345.123)
    )
  }

  it should "validate Symbol" in {
    parseEDNFirst("foo.foo2/bar").map(validate[EDNSymbol]).success.value should be (
      play.api.data.mapping.Success(EDNSymbol("foo.foo2/bar", Some("foo.foo2")))
    )
  }

  it should "validate Keyword" in {
    parseEDNFirst(":foo.foo2/bar").map(validate[EDNKeyword]).success.value should be (
      play.api.data.mapping.Success(EDNKeyword(EDNSymbol("foo.foo2/bar", Some("foo.foo2"))))
    )
  }

  it should "validate List[Long]" in {
    parseEDNFirst("""(1 2 3 4 5)""").map(validate[List[Long]]).success.value should be (
      play.api.data.mapping.Success(List(1L, 2L, 3L, 4L, 5L))
    )

    parseEDNFirst("""(1 2 3 "toto" 5)""").map(validate[List[Long]]).success.value should be (
      play.api.data.mapping.Failure(Seq(Path \ 3 -> Seq(ValidationError("error.number", "Long"))))
    )
  }

  it should "validate Seq[Long]" in {
    parseEDNFirst("""(1 2 3 4 5)""").map(validate[Seq[Long]]).success.value should be (
      play.api.data.mapping.Success(List(1L, 2L, 3L, 4L, 5L))
    )
  }

  it should "validate Vector[Long]" in {
    parseEDNFirst("""[1 2 3 4 5]""").map(validate[Vector[Long]]).success.value should be (
      play.api.data.mapping.Success(Vector(1L, 2L, 3L, 4L, 5L))
    )
    parseEDNFirst("""[1 2 3 "toto" 5]""").map(validate[Vector[Long]]).success.value should be (
      play.api.data.mapping.Failure(Seq(Path \ 3 -> Seq(ValidationError("error.number", "Long"))))
    )
  }

  it should "validate Set[Long]" in {
    parseEDNFirst("""#{1 2 3 4 5}""").map(validate[Set[Long]]).success.value should be (
      play.api.data.mapping.Success(Set(1L, 2L, 3L, 4L, 5L))
    )
    parseEDNFirst("""#{1 2 3 "toto" 5}""").map(validate[Set[Long]]).success.value should be (
      play.api.data.mapping.Failure(Seq(Path \ "toto" -> Seq(ValidationError("error.number", "Long"))))
    )
  }

  it should "validate Map[String, Long]" in {
    parseEDNFirst("""{ "toto" 1 "tata" 2 "tutu" 3 }""").map(validate[Map[String, Long]]).success.value should be (
      play.api.data.mapping.Success(Map(
        "toto" -> 1L,
        "tata" -> 2L,
        "tutu" -> 3L
      ))
    )
  }

  it should "validate Map[Long, String]" in {
    parseEDNFirst("""{ 1 "toto" 2 "tata" 3 "tutu" }""").map(validate[Map[Long, String]]).success.value should be (
      play.api.data.mapping.Success(Map(
        1L -> "toto",
        2L -> "tata",
        3L -> "tutu"
      ))
    )
  }

  it should "validate Option[Long]" in {
    parseEDNFirst("""{ "toto" 1 "tata" 2 "tutu" 3 }""").map(
      (Path \ "tata").read[EDN, Option[Long]].validate
    ).success.value should be (
      play.api.data.mapping.Success(Some(2))
    )
  }

  it should "validate deep path Double" in {
    parseEDNFirst("""{ "toto" 1, "tata" { "foo" 2.123, "bar" 3 }, "tutu" 3 }""").map(
      (Path \ "tata" \ "foo").read[EDN, Double].validate
    ).success.value should be (
      play.api.data.mapping.Success(2.123)
    )
  }


  it should "validate hlist" in {
    import shapeless._
    import scaledn.EDNNil

    parseEDNFirst("""(1 "toto" true nil)""").map(
      validate[Long :: String :: Boolean :: EDNNil.type :: HNil]
    ).success.value should be (
      play.api.data.mapping.Success(1L :: "toto" :: true :: EDNNil :: HNil)
    )

    parseEDNFirst("""[1 "toto" true nil]""").map(
      validate[Long :: String :: Boolean :: EDNNil.type :: HNil]
    ).success.value should be (
      play.api.data.mapping.Success(1L :: "toto" :: true :: EDNNil :: HNil)
    )
  }
}