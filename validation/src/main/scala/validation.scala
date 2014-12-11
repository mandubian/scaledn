package scaledn.validation

import play.api.libs.functional._
import play.api.libs.functional.syntax._
import play.api.data.mapping._

import scaledn._

object Rules extends play.api.data.mapping.DefaultRules[EDN] {

  private def ednAs[T](f: PartialFunction[EDN, Validation[ValidationError, T]])(msg: String, args: Any*) =
    Rule.fromMapping[EDN, T](
      f.orElse {
        case j => Failure(Seq(ValidationError(msg, args: _*)))
      })

  implicit def stringR = ednAs[String] {
    case s: String => Success(s)
  }("error.invalid", "String")

  implicit def booleanR = ednAs[Boolean] {
    case b: Boolean => Success(b)
  }("error.invalid", "Boolean")

  implicit def longR = ednAs[Long] {
    case l: Long => Success(l)
  }("error.number", "Long")

  implicit def bigIntR = ednAs[BigInt] {
    case b: BigInt => Success(b)
  }("error.number", "BigInt")

  implicit def intR = ednAs[Int] {
    case l: Long => Success(l.toInt)
    case b: BigInt if b.isValidInt => Success(b.toInt)
  }("error.number", "Int")

  implicit def shortR = ednAs[Short] {
    case l: Long => Success(l.toShort)
    case b: BigInt if b.isValidShort => Success(b.toShort)
  }("error.number", "Short")

  implicit def doubleR = ednAs[Double] {
    case d: Double => Success(d)
  }("error.number", "Double")

  implicit def BigDecimalR = ednAs[BigDecimal] {
    case b: BigDecimal => Success(b)
  }("error.number", "BigDecimal")

  // BigDecimal.isValidFloat is buggy, see [SI-6699]
  import java.{ lang => jl }
  private def isValidFloat(bd: BigDecimal) = {
    val d = bd.toFloat
    !d.isInfinity && bd.bigDecimal.compareTo(new java.math.BigDecimal(jl.Float.toString(d), bd.mc)) == 0
  }

  implicit def floatR = ednAs[Float] {
    case d: Double => Success(d.toFloat)
    case b: BigDecimal if(isValidFloat(b)) => Success(b.toFloat)
  }("error.number", "Float")

  implicit def symbolR = ednAs[EDNSymbol] {
    case s: EDNSymbol => Success(s)
  }("error.invalid", "Symbol")

  implicit def keywordR = ednAs[EDNKeyword] {
    case k: EDNKeyword => Success(k)
  }("error.invalid", "Keyword")

  implicit def listR = ednAs[List[EDN]] {
    case s: List[EDN] => Success(s)
  }("error.invalid", "List")

  implicit def setR = ednAs[Set[EDN]] {
    case s: Set[EDN @unchecked] => Success(s)
  }("error.invalid", "Set")

  implicit def vectorR = ednAs[Vector[EDN]] {
    case v: Vector[EDN] => Success(v)
  }("error.invalid", "Vector")

  implicit def mapR = ednAs[Map[EDN, EDN]] {
    case m: Map[EDN @unchecked, EDN @unchecked] => Success(m)
  }("error.invalid", "Map")

  implicit def mapKVR[K, V](rk: RuleLike[EDN, K], rv: RuleLike[EDN, V], p: RuleLike[EDN, Map[EDN, EDN]]): Rule[EDN, Map[K, V]] = {
    Rule.toRule(p).compose(Path)(
      Rule { kvs =>
        val rkr = Rule.toRule(rk)
        val rvr = Rule.toRule(rv)
        val validations = kvs.toSeq.map { case kv =>
          ( rkr.repath((Path \ kv._1.toString) ++ _) ~
            rvr.repath((Path \ kv._1.toString) ++ _)
          ).tupled.validate(kv)
        }
        Validation.sequence(validations).map(_.toMap)
      })
  }

  implicit def pickInEdn[II <: EDN, O](p: Path)(implicit r: RuleLike[EDN, O]): Rule[II, O] = {

    def search(path: Path, edn: EDN): Option[EDN] = path.path match {
      case KeyPathNode(k) :: t =>
        edn match {
          case m: Map[EDN @unchecked, EDN @unchecked] =>
            m.find(_._1.toString == k).flatMap(kv => search(Path(t), kv._2))
          case _ => None
        }
      case IdxPathNode(i) :: t =>
        edn match {
          case l: List[EDN] => l.lift(i).flatMap(j => search(Path(t), j))
          case _ => None
        }
      case Nil => Some(edn)
    }

    Rule[II, EDN] { edn =>
      search(p, edn) match {
        case None => Failure(Seq(Path -> Seq(ValidationError("error.required"))))
        case Some(edn) => Success(edn)
      }
    }.compose(r)
  }

  import scala.collection.generic.CanBuildFrom

  def traverse[E, A, B, M[X] <: TraversableOnce[X]](vs: M[Validation[E, A]])(
    implicit cbf: CanBuildFrom[M[A], A, M[A]]
  ): Validation[E, M[A]] = {
    vs.foldLeft[Validation[E, M[A]]](Success(cbf().result)) {
      case (Success(as),  Success(a))   => Success((cbf(as) += a).result)
      case (Success(_),   Failure(e))   => Failure(e)
      case (Failure(e),   Success(_))   => Failure(e)
      case (Failure(e1),  Failure(e2))  => Failure(e1 ++ e2)
    }
  }

  // def travR[M[_] <: Iterable[_], I, O](implicit r: RuleLike[I, O], cbf: CanBuildFrom[EDN, O]): Rule[M[I], M[O]] =
  //   Rule { is: M[I] =>
  //       val rr = Rule.toRule(r)
  //       val withI = is.zipWithIndex.map {
  //         case (v, i) =>
  //           rr.repath((Path \ i) ++ _).validate(v)
  //       }
  //       Validation.sequence(withI)
  //   }


  // private def pickInM[M[_], T](implicit r: RuleLike[M[EDN], M[T]): Rule[JsValue, T] =
  //   jsArrayR.fmap { case JsArray(fs) => fs }.compose(r)

  // implicit def pickList[O](implicit r: RuleLike[EDN, O]) = listR.compose(super.listR[EDN, O])

}