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


  it should "validate Seq[Long]" in {
    EDN.parseFirst("(1 2 3 4 5)").map(validate[List[Long]]).success.value should be (
      play.api.data.mapping.Success(List(1L, 2L, 3L, 4L, 5L))
    )
  }


}