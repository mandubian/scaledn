package scaledn
package macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import parser._
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import shapeless.{HList, HNil}

trait EDNMacros {

  def EDN(edn: String) = macro MacroImpl.ednImpl
  def EDNs(edn: String) = macro MacroImpl.ednsImpl
  def EDNH(edn: String) = macro MacroImpl.ednhImpl
  def EDNHs(edn: String) = macro MacroImpl.ednhsImpl
  def EDNHR(edn: String) = macro MacroImpl.ednhrImpl
  def EDNHRs(edn: String) = macro MacroImpl.ednhrsImpl
}

object MacroImpl {

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
          case TrySuccess(s) => c.Expr(helper.literalEDN(s))
          case TryFailure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case TryFailure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
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
          case TrySuccess(s) => c.Expr(helper.literalEDN(s))
          case TryFailure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case TryFailure(e) => abortWithMessage(c, "Unexpected validation failure: " + e.getMessage)
        }
    }
  }

  def ednhImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) =>
        val parser = EDNParser(s)
        parser.Root.run().map(_.head) match {
          case TrySuccess(s) => c.Expr(helper.literalEDNH(s))
          case TryFailure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case TryFailure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
        }
    }
  }

  def ednhsImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) =>
        val parser = EDNParser(s)
        parser.Root.run() match {
          case TrySuccess(s) => c.Expr(helper.literalEDNHS(s))
          case TryFailure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case TryFailure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
        }
    }
  }

  def ednhrImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) =>
        val parser = EDNParser(s)
        parser.Root.run().map(_.head) match {
          case TrySuccess(s) => c.Expr(helper.literalEDNHR(s))
          case TryFailure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case TryFailure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
        }
    }
  }

  def ednhrsImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) =>
        val parser = EDNParser(s)
        parser.Root.run() match {
          case TrySuccess(s) => c.Expr(helper.literalEDNHRS(s))
          case TryFailure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case TryFailure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
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


  def literalEDNH(edn: EDN): c.Tree = {
    edn match {
      case list: List[EDN] => literalEDNHS(list)
      case vector: Vector[EDN] => literalEDNHS(vector)
      case set: Set[EDN @unchecked] => literalEDNHS(set.toSeq)
      case map: Map[EDN @unchecked, EDN @unchecked] => literalRecords(map.toSeq)
      case x => literalEDN(x)
    }
  }

  def literalEDNHR(edn: EDN): c.Tree = {
    edn match {
      case list: List[EDN] => literalEDNHS(list)
      case vector: Vector[EDN] => literalEDNHS(vector)
      case set: Set[EDN @unchecked] => literalEDNHS(set.toSeq)
      case map: Map[EDN @unchecked, EDN @unchecked] => literalRecordsR(map.toSeq)
      case x => literalEDNH(x)
    }
  }

  def literalEDNHS(edns: Seq[EDN]): c.Tree = {
    edns match {
      case Seq() => literalHNil
      case head +: tail => literalHL(literalEDN(head), literalEDNHS(tail))
    }
  }

  def literalEDNHRS(edns: Seq[EDN]): c.Tree = {
    edns match {
      case Seq() => literalHNil
      case head +: tail => literalHL(literalEDNHR(head), literalEDNHRS(tail))
    }
  }

  def literalRecords(edns: Seq[(EDN, EDN)]): c.Tree = {
    edns match {
      case Seq() => literalHNil
      case (k, v) +: tail => literalHL(literalRecord(literalEDN(k), literalEDN(v)), literalRecords(tail))
    }
  }

  def literalRecordsR(edns: Seq[(EDN, EDN)]): c.Tree = {
    edns match {
      case Seq() => literalHNil
      case (k, v) +: tail => literalHL(literalRecord(literalEDNHR(k), literalEDNHR(v)), literalRecordsR(tail))
    }
  }

  def literalHNil: c.Tree = q"_root_.shapeless.HNil"

  def literalHL(head: c.Tree, tail: c.Tree): c.Tree = q"_root_.shapeless.::($head, $tail)"

  def literalRecord(key: c.Tree, value: c.Tree): c.Tree =
    q"_root_.shapeless.syntax.singleton.mkSingletonOps($key).->>($value)"

}

