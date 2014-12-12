package scaledn.validation

import org.scalatest._

import scala.util.{Try, Success, Failure}

import scaledn._
import scaledn.validation._
import play.api.data.mapping._

import play.api.data.mapping.{Success => VS}

class ValidationSpec extends FlatSpec with Matchers with TryValues {

  import scaledn.validation.Rules._

  def validate[T](edn: EDN)(implicit r: Rule[EDN, T]): Validation[EDN, T] = r.validate(edn)

  "EDN Validation" should "validate String" in {
    EDN.parseFirst("\"foobar\"").map(validate[String]).success.value should be (
      play.api.data.mapping.Success("foobar")
    )

    EDN.parseFirst("\"foobar\"").map(Path.read[EDN, String].validate).success.value should be (
      play.api.data.mapping.Success("foobar")
    )

    EDN.parseFirst("\"foobar\"").map(From[EDN]{ __ => __.read[String] }.validate).success.value should be (
      play.api.data.mapping.Success("foobar")
    )
  }

  it should "validate Long" in {
    EDN.parseFirst("12345").map(validate[Long]).success.value should be (
      play.api.data.mapping.Success(12345L)
    )
  }

  it should "validate Double" in {
    EDN.parseFirst("12345.123").map(validate[Double]).success.value should be (
      play.api.data.mapping.Success(12345.123)
    )
  }

  it should "validate Symbol" in {
    EDN.parseFirst("foo.foo2/bar").map(validate[EDNSymbol]).success.value should be (
      play.api.data.mapping.Success(EDNSymbol("foo.foo2/bar", Some("foo.foo2")))
    )
  }

  it should "validate Keyword" in {
    EDN.parseFirst(":foo.foo2/bar").map(validate[EDNKeyword]).success.value should be (
      play.api.data.mapping.Success(EDNKeyword(EDNSymbol("foo.foo2/bar", Some("foo.foo2"))))
    )
  }

  it should "validate List[Long]" in {
    EDN.parseFirst("""(1 2 3 4 5)""").map(validate[List[Long]]).success.value should be (
      play.api.data.mapping.Success(List(1L, 2L, 3L, 4L, 5L))
    )

    EDN.parseFirst("""(1 2 3 "toto" 5)""").map(validate[List[Long]]).success.value should be (
      play.api.data.mapping.Failure(Seq(Path \ 3 -> Seq(ValidationError("error.number", "Long"))))
    )
  }

  it should "validate Seq[Long]" in {
    EDN.parseFirst("""(1 2 3 4 5)""").map(validate[Seq[Long]]).success.value should be (
      play.api.data.mapping.Success(List(1L, 2L, 3L, 4L, 5L))
    )
  }

  it should "validate Vector[Long]" in {
    EDN.parseFirst("""[1 2 3 4 5]""").map(validate[Vector[Long]]).success.value should be (
      play.api.data.mapping.Success(Vector(1L, 2L, 3L, 4L, 5L))
    )
    EDN.parseFirst("""[1 2 3 "toto" 5]""").map(validate[Vector[Long]]).success.value should be (
      play.api.data.mapping.Failure(Seq(Path \ 3 -> Seq(ValidationError("error.number", "Long"))))
    )
  }

  it should "validate Set[Long]" in {
    EDN.parseFirst("""#{1 2 3 4 5}""").map(validate[Set[Long]]).success.value should be (
      play.api.data.mapping.Success(Set(1L, 2L, 3L, 4L, 5L))
    )
    EDN.parseFirst("""#{1 2 3 "toto" 5}""").map(validate[Set[Long]]).success.value should be (
      play.api.data.mapping.Failure(Seq(Path \ "toto" -> Seq(ValidationError("error.number", "Long"))))
    )
  }

  it should "validate Map[String, Long]" in {
    EDN.parseFirst("""{ "toto" 1 "tata" 2 "tutu" 3 }""").map(validate[Map[String, Long]]).success.value should be (
      play.api.data.mapping.Success(Map(
        "toto" -> 1L,
        "tata" -> 2L,
        "tutu" -> 3L
      ))
    )
  }

  it should "validate Map[Long, String]" in {
    EDN.parseFirst("""{ 1 "toto" 2 "tata" 3 "tutu" }""").map(validate[Map[Long, String]]).success.value should be (
      play.api.data.mapping.Success(Map(
        1L -> "toto",
        2L -> "tata",
        3L -> "tutu"
      ))
    )
  }

  it should "validate Option[Long]" in {
    EDN.parseFirst("""{ "toto" 1 "tata" 2 "tutu" 3 }""").map(
      (Path \ "tata").read[EDN, Option[Long]].validate
    ).success.value should be (
      play.api.data.mapping.Success(Some(2))
    )
  }

  it should "validate deep path Double" in {
    EDN.parseFirst("""{ "toto" 1, "tata" { "foo" 2.123, "bar" 3 }, "tutu" 3 }""").map(
      (Path \ "tata" \ "foo").read[EDN, Double].validate
    ).success.value should be (
      play.api.data.mapping.Success(2.123)
    )
  }

}