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
package macros

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

import parser._
import scala.util.{Try, Success => TrySuccess, Failure => TryFailure}
import shapeless.{HList, HNil}


trait EDNMacros {

  /**
    * Macro parsing '''Single''' EDN value mapping collections to scala collection
    *
    * So, heterogenous collection will use `Any`
    *
    * {{{
    * // the type is just for info as it is inferred by scalac macro
    * val list: List[Long] = EDN("""(1 2 3)""")
    * list should equal (List(1L, 2L, 3L))
    *
    * val map = EDN("""{ 1 "toto", 2 "tata", 3 "tutu" }""")
    * map should equal (Map(1L -> "toto", 2L -> "tata", 3L -> "tutu"))
    * }}}
    *
    * You can also use String interpolation mixed with this macro
    * {{{
    * // the types are just for info as it is inferred by scalac macro
    * val l = 123L
    * val s = List("foo", "bar")
* 
    * val r: Long = EDN(s"$$l")
* 
    * val r1: Seq[Any] = EDN(s"($$l $$s)")
    * }}}
    */
  def EDN(edn: String): Any = macro MacroImpl.ednImpl

  /**
    * Macro parsing '''Multiple''' EDN value mapping collections to scala collection
    *
    * So, heterogenous collection will use `Any`
    *
    * {{{
    * // the types are just for info as it is inferred by scalac macro
    * val s: Seq[Any] = EDNs("""(1 2 3) "toto" [true false] :foo/bar""")
    * s should equal (Seq(
    *   Seq(1L, 2L, 3L),
    *   "toto",
    *   Vector(true, false),
    *   EDNKeyword(EDNSymbol("foo/bar", Some("foo")))
    * ))
    * }}}
    *
    * You can also use String interpolation mixed with this macro
    *
    */
  def EDNs(edn: String): Any = macro MacroImpl.ednsImpl

  /**
    * Macro parsing '''Single''' EDN Value mapping collections to heterogenous shapeless HList
    *
    * The conversion of collections to HList is applied at first level only
    * To recursively convert to HList, use recursive macros
    *
    * {{{
    * // the types are just for info as it is inferred by scalac macro
    * val s: Long :: String :: Boolean :: HNil = EDNH("""(1 "toto" true)""")
    * s should equal (1L :: "toto" :: true :: HNil)
    *
    * val s3 = EDNH("""{1 "toto" true 1.234 "foo" (1 2 3)}""")
    * s3 should equal (
    *   1L ->> "toto" ::
    *   true ->> 1.234 ::
    *   "foo" ->> List(1L, 2L, 3L) ::
    *   HNil
    * )
    * }}}
    *
    * You can also use String interpolation mixed with this macro
    * {{{
    * // the types are just for info as it is inferred by scalac macro
    * val l = 123L
    * val s = List("foo", "bar")
    *
    * val r2: Long :: List[String] :: HNil = EDNH(s"($$l $$s)")
    * }}}
    */
  def EDNH(edn: String): Any = macro MacroImpl.ednhImpl

  /**
    * Macro parsing '''Multiple''' EDN Value mapping collections to heterogenous shapeless HList
    *
    * The conversion of collections to HList is applied at first level only
    * To recursively convert to HList, use recursive macros
    *
    * {{{
    * // the type is just for info as it is inferred by scalac macro
    * val s: List[Long] :: String :: Vector[Boolean] :: EDNKeyword.
    * }}}
    */
  def EDNHs(edn: String): Any = macro MacroImpl.ednhsImpl

  /**
    * Macro parsing '''Single''' EDN Value mapping '''recursively''' collections
    * to heterogenous shapeless HList
    *
    * {{{
    * // the type is just for info as it is inferred by scalac macro
    * val s: List[Long] :: String :: Vector[Boolean] :: EDNKeyword :: HNil = EDNHs("""(1 2 3) "toto" [true false] :foo/bar""")
    * s should equal (
    *   Seq(1L, 2L, 3L) ::
    *   "toto" ::
    *   Vector(true, false) ::
    *   EDNKeyword(EDNSymbol("foo/bar", Some("foo"))) ::
    *   HNil
    * )
    * }}}
    *
    * You can also use String interpolation mixed with this macro.
    *
    */
  def EDNHR(edn: String): Any = macro MacroImpl.ednhrImpl

  /**
    * Macro parsing '''Multiple''' EDN Value mapping '''recursively''' collections
    * to heterogenous shapeless HList
    *
    * {{{
    * // the type is just for info as it is inferred by scalac macro
    * val s2 = EDNHRs("""(1 2 3) "toto" [true false] :foo/bar""")
    * s2 should equal (
    *   (1L :: 2L :: 3L :: HNil) ::
    *   "toto" ::
    *   (true :: false :: HNil) ::
    *   EDNKeyword(EDNSymbol("foo/bar", Some("foo"))) ::
    *   HNil
    * )
    * }}}
    *
    * You can also use String interpolation mixed with this macro.
    *
    */
  def EDNHRs(edn: String): Any = macro MacroImpl.ednhrsImpl
}

object MacroImpl {
  import scala.collection.mutable

  private def abortWithMessage(c: Context, message: String) =
    c.abort(c.enclosingPosition, message)

  private def abortWithThrowable(c: Context, throwable: Throwable) =
    c.abort(c.enclosingPosition, throwable.getMessage)

  def genericMacro[T](c: Context)(edn: c.Expr[String])
      (parse: EDNParser => Try[T])
      (literal: (T, Helper[c.type], mutable.Stack[c.Tree]) => c.Tree): c.Expr[Any] =
  {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) =>
        val parser = EDNParser(s)
        parse(parser) match {
          case TrySuccess(s) => c.Expr(literal(s, helper, mutable.Stack.empty[c.Tree]))
          case TryFailure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case TryFailure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
        }

      case s@q"scala.StringContext.apply(..$parts).s(..$args)" =>
        val partsWithPlaceholders = q"""Seq(..$parts).mkString(" scaledn/! ")"""
        val strWithPlaceHolders = c.eval(c.Expr[String](c.untypecheck(partsWithPlaceholders.duplicate)))
        val parser = EDNParser(strWithPlaceHolders)
        val argsStack = mutable.Stack.concat(args)
        parse(parser) match {
          case TrySuccess(s) => c.Expr(literal(s, helper, argsStack))
          case TryFailure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case TryFailure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
        }

      case _ =>
        abortWithMessage(c, "Expected a string literal")
    }
  }

  def ednImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] = 
    genericMacro[EDN](c)(edn){
      parser => parser.Root.run().map(_.head)
    }{ 
      (t, helper, stack) => helper.literalEDN(t, stack)
    }


  def ednsImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[Seq[EDN]](c)(edn){
      parser => parser.Root.run()
    }{ 
      (t, helper, stack) => helper.literalEDN(t, stack)
    }


  def ednhImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[EDN](c)(edn){
      parser => parser.Root.run().map(_.head)
    }{ 
      (t, helper, stack) => helper.literalEDNH(t, stack)
    }


  def ednhsImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[Seq[EDN]](c)(edn){
      parser => parser.Root.run()
    }{ 
      (t, helper, stack) => helper.literalEDNHS(t, stack)
    }


  def ednhrImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[EDN](c)(edn){
      parser => parser.Root.run().map(_.head)
    }{ 
      (t, helper, stack) => helper.literalEDNHR(t, stack)
    }


  def ednhrsImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[Seq[EDN]](c)(edn){
      parser => parser.Root.run()
    }{ 
      (t, helper, stack) => helper.literalEDNHRS(t, stack)
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

  def literalEDN(edn: Any, stk: mutable.Stack[c.Tree]): c.Tree =
    edn match {
      case s: String => q"$s"
      case b: Boolean => q"$b"
      case l: Long => q"$l"
      case d: Double => q"$d"
      case bi: BigInt => q"$bi"
      case bd: BigDecimal => q"$bd"
      case s: EDNSymbol => literalEDNSymbol(s, stk) //q"_root_.scaledn.EDNSymbol(${s.value}, ${s.namespace})"
      case kw: EDNKeyword => literalEDNKeyword(kw, stk)
      case EDNNil => q"_root_.scaledn.EDNNil"
      case list: List[EDN] =>
        val args = list.map(literalEDN(_, stk))
        q"_root_.scala.collection.immutable.List(..$args)"
      case vector: Vector[EDN] =>
        val args = vector.map(literalEDN(_, stk))
        q"_root_.scala.collection.immutable.Vector(..$args)"
      case set: Set[EDN @unchecked] =>
        val args = set.map(literalEDN(_, stk))
        q"_root_.scala.collection.immutable.Set(..$args)"
      case map: Map[EDN @unchecked, EDN @unchecked] =>
        val args = map.map{ case(k, v) => literalEDN(k, stk) -> literalEDN(v, stk) }
        q"_root_.scala.collection.immutable.Map(..$args)"
      case seq: Seq[EDN] =>
        val args = seq.map(literalEDN(_, stk))
        q"_root_.scala.collection.immutable.Seq(..$args)"
      case t: EDNTagged[EDN @unchecked] =>
        val tag = literalEDNSymbol(t.tag, stk)
        val value = literalEDN(t.value, stk)
        q"_root_.scaledn.EDNTagged($tag, $value)"
      case x =>
        // TODO Add search implicit for this element
        if (x == null)
          abortWithMessage("null is not supported")
        else
          abortWithMessage(s"unexpected value $x with ${x.getClass}")
    }

  def literalEDNR(edn: Any, stk: mutable.Stack[c.Tree]): c.Tree =
    edn match {
      case s: String => q"$s"
      case b: Boolean => q"$b"
      case l: Long => q"$l"
      case d: Double => q"$d"
      case bi: BigInt => q"$bi"
      case bd: BigDecimal => q"$bd"
      case s: EDNSymbol => literalEDNSymbol(s, stk) //q"_root_.scaledn.EDNSymbol(${s.value}, ${s.namespace})"
      case kw: EDNKeyword => literalEDNKeyword(kw, stk)
      case EDNNil => q"_root_.scaledn.EDNNil"
      case list: List[EDN] =>
        literalEDNHS(list, stk)
      case vector: Vector[EDN] =>
        literalEDNHS(vector, stk)
      case set: Set[EDN @unchecked] =>
        literalEDNHS(set.toSeq, stk)
      case map: Map[EDN @unchecked, EDN @unchecked] =>
        literalRecords(map.toSeq, stk)
      case seq: Seq[EDN] =>
        literalEDNHS(seq, stk)
      case t: EDNTagged[EDN @unchecked] =>
        val tag = literalEDNSymbol(t.tag, stk)
        val value = literalEDNR(t.value, stk)
        println("VALUE:"+value)
        q"_root_.scaledn.EDNTagged($tag, $value)"
      case x =>
        if (x == null)
          abortWithMessage("nil is not supported")
        else
          abortWithMessage(s"unexpected value $x with ${x.getClass}")
    }

  def literalEDNKeyword(kw: EDNKeyword, stk: mutable.Stack[c.Tree]): c.Tree =
    q"_root_.scaledn.EDNKeyword(${literalEDNSymbol(kw.value, stk)})"

  def literalEDNSymbol(s: EDNSymbol, stk: mutable.Stack[c.Tree]): c.Tree = {
    if (s.value == "scaledn/!")
      try {
        val t = stk.pop()
        q"""$t"""
      } catch {
        case ex: NoSuchElementException =>
          abortWithMessage("The symbol 'scaledn/!' is reserved by Scaledn")
      }
    else
      q"_root_.scaledn.EDNSymbol(${s.value}, ${s.namespace})"

  }

  def literalEDNH(edn: EDN, stk: mutable.Stack[c.Tree]): c.Tree = {
    edn match {
      case list: List[EDN] => literalEDNHS(list, stk)
      case vector: Vector[EDN] => literalEDNHS(vector, stk)
      case set: Set[EDN @unchecked] => literalEDNHS(set.toSeq, stk)
      case map: Map[EDN @unchecked, EDN @unchecked] => literalRecords(map.toSeq, stk)
      case seq: Seq[EDN] => literalEDNHS(seq, stk)
      case x => literalEDN(x, stk)
    }
  }

  def literalEDNHR(edn: EDN, stk: mutable.Stack[c.Tree]): c.Tree = {
    edn match {
      case list: List[EDN] => literalEDNHRS(list, stk)
      case vector: Vector[EDN] => literalEDNHRS(vector, stk)
      case set: Set[EDN @unchecked] => literalEDNHRS(set.toSeq, stk)
      case map: Map[EDN @unchecked, EDN @unchecked] => literalRecordsR(map.toSeq, stk)
      case seq: Seq[EDN] => literalEDNHRS(seq, stk)
      case x => literalEDNR(x, stk)
    }
  }

  def literalEDNHS(edns: Seq[EDN], stk: mutable.Stack[c.Tree]): c.Tree = {
    edns match {
      case Seq() => literalHNil
      case head +: tail => literalHL(literalEDN(head, stk), literalEDNHS(tail, stk))
    }
  }

  def literalEDNHRS(edns: Seq[EDN], stk: mutable.Stack[c.Tree]): c.Tree = {
    edns match {
      case Seq() => literalHNil
      case head +: tail => literalHL(literalEDNHR(head, stk), literalEDNHRS(tail, stk))
    }
  }

  def literalRecords(edns: Seq[(EDN, EDN)], stk: mutable.Stack[c.Tree]): c.Tree = {
    edns match {
      case Seq() => literalHNil
      case (k, v) +: tail => literalHL(literalRecord(literalEDN(k, stk), literalEDN(v, stk)), literalRecords(tail, stk))
    }
  }

  def literalRecordsR(edns: Seq[(EDN, EDN)], stk: mutable.Stack[c.Tree]): c.Tree = {
    edns match {
      case Seq() => literalHNil
      case (k, v) +: tail => literalHL(literalRecord(literalEDNHR(k, stk), literalEDNHR(v, stk)), literalRecordsR(tail, stk))
    }
  }

  def literalHNil: c.Tree = q"_root_.shapeless.HNil"

  def literalHL(head: c.Tree, tail: c.Tree): c.Tree = q"_root_.shapeless.::($head, $tail)"

  def literalRecord(key: c.Tree, value: c.Tree): c.Tree =
    q"_root_.shapeless.syntax.singleton.mkSingletonOps($key).->>($value)"

}

