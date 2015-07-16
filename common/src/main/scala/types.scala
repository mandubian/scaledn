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


/** AST root type
  * The types represented by the AST are:
  *
  *   - EDN Symbol          foo/bar
  *   - EDN Keyword         :foo/bar
  *   - EDN Nil             nil
  *   - EDN Tagged values   #foo/bar value
  *
  *
  * The following types aren't represented in the AST because the types 
  * described in EDN specs are isomorphic/bijective with Scala types :
  *
  *   - Long (64bits)       12345
  *   - Double (64 bits)    123.45
  *   - BigInt              12345M
  *   - BigDecimal          123.45N
  *   - String              "foobar"
  *   - Characters          \c \newline \return \space \tab \\ \u0308 etc...
  *   - heterogenous list   (1 true "toto")
  *   - heterogenous vector [1 true "toto"]
  *   - heterogenous set    #{1 true "toto"}
  *   - heterogenous map    {1 "toto", 1.234 "toto"}
  *
  * For more info, go to the [[https://github.com/edn-format/edn EDN format site]]
  */
sealed trait EDNValue

sealed trait Namespace
case object NoNS extends Namespace
case class NS(ns: String) extends Namespace

case class Named(name: String, namespace: Namespace = NoNS) {
  override def toString = namespace match {
    case NoNS => name
    case NS(ns) => s"$ns/$name"
  }
}

object Named {
  def parseNs(s: String) = s.split("/") match {
    case Array(v) => Named(v)
    case Array(ns, v) => Named(v, NS(ns))
    case Array("", v) => Named(v)
    case _ => throw new RuntimeException("Bad Named format (ns/)name")
  }

  def apply(s: Symbol) = new Named(s.name)
}

/** EDN Symbol representing an generic identifier with a name and potentially a namespace
  * like:
  * {{{
  * foo.bar/toto
  * }}}
  */
case class EDNSymbol(named: Named) extends EDNValue {
  override def toString = s"$named"
}

/** EDN keyword representing a unique identifier with a name and potentially a namespace
  * like:
  * {{{
  * :foo.bar/toto`
  * }}}
  */
case class EDNKeyword(named: Named) extends EDNValue {
  override def toString = s":$named"
}

/** EDN tagged values look like `#foo.bar/toto 123L` and correspond to the extension
  * mechanism provided by EDN. The tag implies a semantic that should be managed by
  * a handler. By default, EDN provides 2 default handlers:
  *   - #inst "1985-04-12T23:20:50.52Z" for RFC-3339 instants
  *   - #uuid "f81d4fae-7dec-11d0-a765-00a0c91e6bf6" for UUID
  */
case class EDNTagged[A](tag: Named, value: A) extends EDNValue {
  override def toString = s"#${tag} ${value.toString}"
}

/** The EDN Nil value that can represent anything null/nil/nothing you need */
case object EDNNil extends EDNValue {
  override def toString = "nil"
}

/** Unneeded types

case class  EDNBoolean(value: Boolean) extends EDNValue
case class  EDNString(value: String) extends EDNValue
case class  EDNChar(value: Char) extends EDNValue

case class  EDNLong(value: Long) extends EDNValue
case class  EDNBigInt(value: BigInt) extends EDNValue

case class  EDNDouble(value: Double) extends EDNValue
case class  EDNBigDec(value: BigDecimal) extends EDNValue
*/
