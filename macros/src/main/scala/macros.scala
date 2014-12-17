package scaledn
package macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import parser._
import scala.util.{Try, Success, Failure}
import shapeless.{HList, HNil}
import play.api.data.mapping.{RuleLike, Validation, Success, Failure}


trait EDNMacros {

  def edn(edn: String) = macro MacroImpl.ednImpl
  def edns(edn: String) = macro MacroImpl.ednsImpl
  def ednh[HL <: HList](edn: String) = macro MacroImpl.ednhImpl[HL]
}

object MacroImpl extends validate.ShapelessRules {

  private def abortWithMessage(c: Context, message: String) =
    c.abort(c.enclosingPosition, message)

  private def abortWithThrowable(c: Context, throwable: Throwable) =
    c.abort(c.enclosingPosition, throwable.getMessage)

  def ednImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) => 
        val parser = EDNParser(s)
        parser.Root.run().map(_.head) match {
          case Success(s) => c.Expr(helper.literalEDN(s))
          case Failure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case Failure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
        }
    }
  }

  def ednsImpl(c: Context)(edn: c.Expr[String]): c.Expr[Seq[Any]] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) => 
        val parser = EDNParser(s)
        parser.Root.run() match {
          case Success(s) => c.Expr(helper.literalEDN(s))
          case Failure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case Failure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
        }
    }
  }

  def ednhImpl[HL <: HList](c: Context)(edn: c.Expr[String])(implicit r: RuleLike[Seq[EDN], HL]): c.Expr[HL] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) => 
        val parser = EDNParser(s)
        parser.Root.run() match {
          case Success(s) => c.Expr(helper.literalEDNH[HL](s))
          case Failure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case Failure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
        }
    }
  }
}

 
class Helper[C <: Context](val c: C) {
  import c.universe._
  import scala.collection.mutable

  private def abortWithMessage(message: String) =
    c.abort(c.enclosingPosition, message)

  implicit val bigDecimalLiftable = new Liftable[BigDecimal] {
    def apply(n: BigDecimal) =
      c.Expr[BigDecimal](
        Apply(
          q"scala.math.BigDecimal.apply",
          List(Literal(Constant(n.toString))))).tree
  }

  implicit val bigIntLiftable = new Liftable[BigInt] {
    def apply(n: BigInt) =
      c.Expr[BigInt](
        Apply(
          q"scala.math.BigInt.apply",
          List(Literal(Constant(n.toString))))).tree
  }

  def literalEDN(edn: Any, stk: mutable.Stack[c.Tree] = mutable.Stack.empty[c.Tree]): c.Tree =
    edn match {
      case s: String => q"$s"
      case b: Boolean => q"$b"
      case l: Long => q"$l"
      case d: Double => q"$d"
      case bi: BigInt => q"$bi"
      case bd: BigDecimal => q"$bd"
      case s: EDNSymbol => q"_root_.scaledn.EDNSymbol(${s.value}, ${s.namespace})"
      case kw: EDNKeyword => q"_root_.scaledn.EDNKeyword(${literalEDN(kw.value)})"
      case EDNNil => q"_root_.scaledn.EDNNil"
      case list: List[EDN] =>
        val args = list.map(literalEDN(_))
        q"_root_.scala.collection.immutable.List(..$args)"
      case vector: Vector[EDN] =>
        val args = vector.map(literalEDN(_))
        q"_root_.scala.collection.immutable.Vector(..$args)"
      case set: Set[EDN @unchecked] =>
        val args = set.map(literalEDN(_))
        q"_root_.scala.collection.immutable.Set(..$args)"
      case map: Map[EDN @unchecked, EDN @unchecked] =>
        val args = map.map{ case(k, v) => literalEDN(k) -> literalEDN(v) }
        q"_root_.scala.collection.immutable.Map(..$args)"
      case seq: Seq[EDN] =>
        val args = seq.map(literalEDN(_))
        q"_root_.scala.collection.immutable.Seq(..$args)"
      case x =>
        if (x == null)
          abortWithMessage("nil is not supported")
        else
          abortWithMessage(s"unexpected value $x with ${x.getClass}")
    }


  // implicit def hnilR: Seq[EDN] => HNil = {
  //   case Seq() => HNil
  //   case _ => abortWithMessage("non empty list can't be mapped to HNil")
  // }

  // implicit def hlistR[HH, HT <: HList](
  //   implicit hr: Seq[EDN] => HT
  // ): Seq[EDN] => HH :: HT = {
  //   case head +: tail => literalEDN(head) :: hr(tail)
  //   case _ => abortWithMessage("cannot map anything else")
  // }

  def literalEDNH[HL <: HList](edns: Seq[EDN])(implicit r: RuleLike[Seq[EDN], HL]): c.Tree = {
    r.validate(edns) match {
      case Success(s) => literalHL(s)
      case Failure(f) => abortWithMessage(f.toString)
    }
  }

  def literalHL[HL <: HList](hl: HL): c.Tree = hl match {
    case h :: t => 
      val htree = literalEDN(h)
      val htail = literalHL(t)
      q"_root_.shapeless.::($htree, $htail)"
  }
}