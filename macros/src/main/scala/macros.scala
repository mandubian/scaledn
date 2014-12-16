package scaledn
package macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import parser._
import scala.util.{Try, Success, Failure}

trait EDNMacros {

  def edn1(edn: String) = macro MacroImpl.edn1Impl

}

object MacroImpl {

  private def abortWithMessage(c: Context, message: String) =
    c.abort(c.enclosingPosition, message)


  private def abortWithThrowable(c: Context, throwable: Throwable) =
    c.abort(c.enclosingPosition, throwable.getMessage)


  // private def readEDN(c: Context, edn: String): AnyRef =
  //   try {
  //     withClojure { datomic.Util.read(edn) }
  //   } catch {
  //     case ex: RuntimeException =>
  //       abortWithThrowable(c, ex)
  //   }


  def edn1Impl(c: Context)(edn: c.Expr[String]): c.Expr[Any] = {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) => 
        val parser = EDNParser(s)
        parser.Root.run().map(_.head) match {
          case Success(s) => c.Expr(helper.literalEDN(s))
          case Failure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
        }
        
    }
  //       val edn = readEDN(c, s)
  //       validateCljRules(c, edn)
  //       val helper = new Helper[c.type](c)
  //       helper.literalQueryRules(helper.literalEDN(edn))

  //     case q"scala.StringContext.apply(..$parts).s(..$args)" =>
  //       val partsWithPlaceholders = q"""Seq(..$parts).mkString(" ! ")"""
  //       val strWithPlaceHolders = c.eval(c.Expr[String](c.untypecheck(partsWithPlaceholders.duplicate)))
  //       val edn = readEDN(c, strWithPlaceHolders)
  //       validateCljRules(c, edn)
  //       val argsStack = mutable.Stack.concat(args)
  //       val helper = new Helper[c.type](c)
  //       helper.literalQueryRules(helper.literalEDN(edn, argsStack))

  //     case _ =>
  //       abortWithMessage(c, "Expected a string literal")
  //   }
  }

}


class Helper[C <: Context](val c: C) {
  import c.universe._
  import scala.collection.mutable

  private def abortWithMessage(message: String) =
    c.abort(c.enclosingPosition, message)


  def literalEDN(edn: Any, stk: mutable.Stack[c.Tree] = mutable.Stack.empty[c.Tree]): c.Tree =
    edn match {
      case s: String => q"$s"
      case b: Boolean => q"$b"
      case l: Long => q"$l"
      case d: Double => q"$d"
      case bi: BigInt => q"$bi"
      case bd: BigDecimal => q"_root_.clojure.lang.BigInt.fromBigInteger(new _root_._root_.java.math.BigInteger(${k.toString}))"

      case x =>
        if (x == null)
          abortWithMessage("nil is not supported")
        else
          abortWithMessage(s"unexpected value $x with ${x.getClass}")
    }
}