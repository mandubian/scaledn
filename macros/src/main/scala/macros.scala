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
    * Macro parsing **Single** EDN value mapping collections to scala collection
    *
    * So, heterogenous collection will use `Any`
    *
    * ```scala
    * // the type is just for info as it is inferred by scalac macro
    * val list: List[Long] = EDN("""(1 2 3)""")
    * list should equal (List(1L, 2L, 3L))
    *
    * val map = EDN("""{ 1 "toto", 2 "tata", 3 "tutu" }""")
    * map should equal (Map(1L -> "toto", 2L -> "tata", 3L -> "tutu"))
    * ```
    */
  def EDN(edn: String) = macro MacroImpl.ednImpl

  /**
    * Macro parsing **Multiple** EDN value mapping collections to scala collection
    *
    * So, heterogenous collection will use `Any`
    *
    * ```scala
    * // the type is just for info as it is inferred by scalac macro
    * val s: Seq[Any] = EDNs("""(1 2 3) "toto" [true false] :foo/bar""")
    * s should equal (Seq(
    *   Seq(1L, 2L, 3L),
    *   "toto",
    *   Vector(true, false),
    *   EDNKeyword(EDNSymbol("foo/bar", Some("foo")))
    * ))
    * ```
    */
  def EDNs(edn: String) = macro MacroImpl.ednsImpl

  /**
    * Macro parsing **Single** EDN Value mapping collections to heterogenous shapeless HList
    *
    * The conversion of collections to HList is applied at first level only
    * To recursively convert to HList, use recursive macros
    *
    * ```scala
    * // the type is just for info as it is inferred by scalac macro
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
    * ```
    */
  def EDNH(edn: String) = macro MacroImpl.ednhImpl

  /**
    * Macro parsing **Multiple** EDN Value mapping collections to heterogenous shapeless HList
    *
    * The conversion of collections to HList is applied at first level only
    * To recursively convert to HList, use recursive macros
    *
    * ```scala
    * // the type is just for info as it is inferred by scalac macro
    * val s: List[Long] :: String :: Vector[Boolean] :: EDNKeyword :: HNil = EDNHs("""(1 2 3) "toto" [true false] :foo/bar""")
    * s should equal (
    *   Seq(1L, 2L, 3L) ::
    *   "toto" ::
    *   Vector(true, false) ::
    *   EDNKeyword(EDNSymbol("foo/bar", Some("foo"))) ::
    *   HNil
    * )
    * ```
    */
  def EDNHs(edn: String) = macro MacroImpl.ednhsImpl

  /**
    * Macro parsing **Single** EDN Value mapping **recursively** collections
    * to heterogenous shapeless HList
    *
    * ```scala
    * // the type is just for info as it is inferred by scalac macro
    * val s: List[Long] :: String :: Vector[Boolean] :: EDNKeyword :: HNil = EDNHs("""(1 2 3) "toto" [true false] :foo/bar""")
    * s should equal (
    *   Seq(1L, 2L, 3L) ::
    *   "toto" ::
    *   Vector(true, false) ::
    *   EDNKeyword(EDNSymbol("foo/bar", Some("foo"))) ::
    *   HNil
    * )
    * ```
    */
  def EDNHR(edn: String) = macro MacroImpl.ednhrImpl

  /**
    * Macro parsing **Multiple** EDN Value mapping **recursively** collections
    * to heterogenous shapeless HList
    *
    * ```scala
    * // the type is just for info as it is inferred by scalac macro
    * val s2 = EDNHRs("""(1 2 3) "toto" [true false] :foo/bar""")
    * s2 should equal (
    *   (1L :: 2L :: 3L :: HNil) ::
    *   "toto" ::
    *   (true :: false :: HNil) ::
    *   EDNKeyword(EDNSymbol("foo/bar", Some("foo"))) ::
    *   HNil
    * )
    * ```
    */
  def EDNHRs(edn: String) = macro MacroImpl.ednhrsImpl
}

object MacroImpl {

  private def abortWithMessage(c: Context, message: String) =
    c.abort(c.enclosingPosition, message)

  private def abortWithThrowable(c: Context, throwable: Throwable) =
    c.abort(c.enclosingPosition, throwable.getMessage)

  def genericMacro[T](c: Context)(edn: c.Expr[String])
      (parse: EDNParser => Try[T])
      (literal: (T, Helper[c.type]) => c.Tree): c.Expr[Any] =
  {
    import c.universe._

    val helper = new Helper[c.type](c)

    edn.tree match {
      case Literal(Constant(s: String)) =>
        val parser = EDNParser(s)
        parse(parser) match {
          case TrySuccess(s) => c.Expr(literal(s, helper))
          case TryFailure(f : org.parboiled2.ParseError) => abortWithMessage(c, parser.formatError(f))
          case TryFailure(e) => abortWithMessage(c, "Unexpected failure: " + e.getMessage)
        }
    }
  }

  def ednImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] = 
    genericMacro[EDN](c)(edn){
      parser => parser.Root.run().map(_.head)
    }{ 
      (t, helper) => helper.literalEDN(t)
    }


  def ednsImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[Seq[EDN]](c)(edn){
      parser => parser.Root.run()
    }{ 
      (t, helper) => helper.literalEDN(t)
    }


  def ednhImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[EDN](c)(edn){
      parser => parser.Root.run().map(_.head)
    }{ 
      (t, helper) => helper.literalEDNH(t)
    }


  def ednhsImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[Seq[EDN]](c)(edn){
      parser => parser.Root.run()
    }{ 
      (t, helper) => helper.literalEDNHS(t)
    }


  def ednhrImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[EDN](c)(edn){
      parser => parser.Root.run().map(_.head)
    }{ 
      (t, helper) => helper.literalEDNHR(t)
    }


  def ednhrsImpl(c: Context)(edn: c.Expr[String]): c.Expr[Any] =
    genericMacro[Seq[EDN]](c)(edn){
      parser => parser.Root.run()
    }{ 
      (t, helper) => helper.literalEDNHRS(t)
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

