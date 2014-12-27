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
package parser

import org.parboiled2._
import scala.annotation.switch
import org.joda.time.{DateTime, DateTimeZone}


/** The parboiled2 EDN Parser
  * 
  * ```scala
  * val parser = EDNParser("""{1 "foo", "bar" 1.234M, :foo/bar [1,2,3]} #_foo/bar :bar/foo""")
  * parser.Root.run() match {
  *   case Success(t) => \/-(t)
  *   case Failure(f : org.parboiled2.ParseError) => -\/(parser.formatError(f))
  * }
  * ```
  * 
  * The parsed types are the following:
  *
  * - Long (64bits)       12345
  * - Double (64 bits)    123.45
  * - BigInt              12345N
  * - BigDecimal          123.45M
  * - String              "foobar"
  * - EDN Symbol          foo/bar
  * - EDN Keyword         :foo/bar
  * - EDN Nil             nil
  * - heterogenous list   (1 true "toto")
  * - heterogenous vector [1 true "toto"]
  * - heterogenous set    #{1 true "toto"}
  * - heterogenous map    {1 "toto", 1.234 "toto"}
  *
  * There are special syntaxes:
  *
  * - comments are lines starting with `;`
  * - values starting with `#_` are parsed but discarded
  * 
  * EDN is an extensible format using tags starting with `#` such as:
  *
  * ```
  * #foo/bar value
  * ```
  * 
  * The parser can provide tag handlers that can be applied when a tag is parsed.
  * EDN specifies 2 tag handlers:
  * 
  * - `#inst "1985-04-12T23:20:50.52Z"` for RFC-3339 instants
  * -  `#uuid "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"` for UUID
  *
  *
  * The parser can also be extended with your own specific handlers:
  * 
  *
  * ```scala
  * val parser = new EDNParser("""#foo bar""") {
  *   // defines your own handler as a parboiled2 rule
  *   val fooTag = rule("foo" ~ WS ~ "bar" ~ push("toto"))
  *
  *   // override tags keeping default tags if you need them
  *   override def tags = rule(fooTag | super.tags) 
  * }
  *
  * parser.Root.run().success.value should be (
  *   Vector("toto")
  * )
  * ```
  *
  */
object EDNParser {
  def apply(input: ParserInput) = new EDNParser(input)
}

class EDNParser(val input: ParserInput) extends Parser with StringBuilding {
  import CharPredicate.{Digit, Digit19, HexDigit, Alpha, AlphaNum}

  /** automatically consumes whitespaces */
  implicit def wspStr(s: String): Rule0 = rule( str(s) ~ WS )

  /** the main rule to be called when parsing all EDN */
  def Root: Rule1[Seq[Any]] = rule( oneOrMore(Elem) ~ EOI )

  /** as shown in parboiled2 samples, here is a (premature) optimization
    * that checks first character before dispatching to the right rule
    */
  def Elem: Rule1[Any] = rule (
    SkipWS ~ run (
      (cursorChar: @switch) match {
        case '"'  => String
        case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '+' => Double | Long
        case ':'  => Keyword
        case '('  => List
        case '{'  => Map
        case '['  => Vector
        case '#'  => Set | Tagged
        case 't'  => True | Symbol
        case 'f'  => False | Symbol
        case 'n'  => Nil | Symbol
        case '\\' => Char
        case _    => Symbol
      }
    ) ~ SkipWS
  )

  /**
    * NIL
    */
  def Nil = rule { "nil" ~ push(EDNNil)}

  /**
    * BOOLEAN
    */
  def True = rule { "true" ~ push(true) }
  def False = rule { "false" ~ push(false) }

  def Boolean = rule { True | False }

  /**
    * LONG 12345 / 12345M
    */
  def Long = rule(
    capture(SignedNumber) ~ LongExact
  )
  def LongExact = rule(
    ch('N') ~> ( (s:String) => BigInt(s) )
    | run( (s:String) => java.lang.Long.parseLong(s) )
  )
  def SignedNumber = rule( optional(anyOf("+-")) ~ Number )

  def Number = rule(
    Digit19 ~ Digits | Digit
  )
  def Digits = rule( oneOrMore(Digit) )

  /**
    * DOUBLE 123.45e+9 / 1.23456789N
    */
  def Double = rule(
    capture(SignedNumber ~ FracExp) ~ DoubleExact
  )
  def DoubleExact = rule(
      ch('M') ~> ( (s:String) => scala.BigDecimal(s) )
    | run( (s:String) => java.lang.Double.parseDouble(s) )
  )
  def FracExp = rule(
      Frac ~ Exp |
      Frac |
      Exp
  )
  def Frac = rule( ch('.') ~ Digits )
  def Exp = rule( Ex ~ Digits )
  def Ex = rule( ignoreCase('e') ~ optional(anyOf("+-")) )

  /**
    * STRING "foobar"
    */
  def String = rule ( '"' ~ clearSB() ~ Characters ~ '"' ~ push(sb.toString) )

  def Characters = rule ( zeroOrMore(NormalChar | '\\' ~ EscapedChar) )
  def NormalChar = rule ( !QuoteBackSlash ~ ANY ~ appendSB() )

  def EscapedChar = rule (
      'n'     ~ appendSB('\n')
    | 'r'     ~ appendSB('\r')
    | 't'     ~ appendSB('\t')
    | '"'     ~ appendSB('\"')
    | '\''    ~ appendSB('\'')
    | '\\'    ~ appendSB('\\')
    | 'b'     ~ appendSB('\b')
    | 'f'     ~ appendSB('\f')
    | Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
  )

  /**
    * CHARACTER \c \newline ...
    */
  def Char = rule { '\\' ~ Chars }

  def Chars = rule (
      "newline" ~ push('\n')
    | "return"  ~ push('\r')
    | "space"   ~ push(' ')
    | "tab"     ~ push('\t')
    | "\\"      ~ push('\\')
    | Unicode   ~> { code => push(code.asInstanceOf[Char]) }
    | AlphaNum  ~ push(lastChar)
  )

  /**
    * KEYWORD
    */
  def Keyword = rule( ':' ~ !CharPredicate(":/") ~ clearSB() ~ NamedRule ~> (EDNKeyword(_)) )

  /**
    * SYMBOL
    */
  def Symbol = rule( 
    !CharPredicate(":#;") ~ clearSB() ~ NamedRule ~> (EDNSymbol(_))
  )

  def NamedRule = rule(
    optional(NamedNameSpace ~ push(sb.toString)) ~ clearSB() ~
    NamedString ~ push(sb.toString) ~> { (ns, value) => Named(value, ns.map(NS(_)).getOrElse(NoNS)) }
  )

  def NamedNameSpace = rule(NamedString ~ "/")
  def NamedString = rule(
      NamedSpecial
    | NamedFirstChars ~ appendSB() ~ zeroOrMore(NamedChars ~ appendSB())
  )
  def NamedSpecial = rule(
    NamedSpecialStart ~ zeroOrMore(NamedChars ~ appendSB())
  )
  def NamedSpecialStart = rule(
    capture(NamedSpecialFirstChars ~ !Digit) ~> ((s:String) => appendSB(s)) 
  )
  def NamedFirstChars = rule(Alpha | NamedNonAlphaChars)
  def NamedChars = rule(AlphaNum | NamedNonAlphaChars | NamedSpecialFirstChars)
  // non alpha chars without +-.
  val NamedNonAlphaChars = CharPredicate("*!_?$%&=<>:#")
  val NamedSpecialFirstChars = CharPredicate("+-.")

  def Unicode = rule ( 'u' ~ capture(HexDigit ~ HexDigit ~ HexDigit ~ HexDigit) ~>
    (java.lang.Integer.parseInt(_, 16))
  )
  
  /**
    * TAGGED
    */
  def Tagged = rule(
    ch('#') ~ tags
  )

  // the rule to override if you want more handlers
  def tags = rule( defaultTags | unknownTags )

  // default tag handlers
  def defaultTags = rule(uuid | instant)

  // This rule can consume any tag even if there is no handler for it
  def unknownTags = rule(NamedRule ~ WS ~ Elem ~> ( EDNTagged(_, _) ))

  def uuid = rule("uuid" ~ WS ~ String ~> (java.util.UUID.fromString(_)))
  def instant = rule("inst" ~ WS ~ String ~> (new DateTime(_, DateTimeZone.UTC)))

  /**
    * DISCARD
    */
  def Discard = rule(
    str("#_") ~ WS ~ Elem ~> (_ => ())
  )

  /**
    * LIST
    */
  def List = rule(
    ch('(') ~ zeroOrMore(Elem) ~ ch(')') ~> ( scala.collection.immutable.List(_:_*) )
  )

  /**
    * VECTOR
    */
  def Vector = rule(
    ch('[') ~ zeroOrMore(Elem) ~ ch(']') ~> ( scala.collection.immutable.Vector(_:_*) )
  )

  /**
    * SET
    */
  def Set = rule(
    str("#{") ~ zeroOrMore(Elem) ~ ch('}') ~ run { 
      elements: Seq[Any] => test(elements.distinct.size == elements.size) ~ push(scala.collection.immutable.Set(elements:_*))
    }
  )


  /**
    * MAP
    */
  def Map = rule(
    ch('{') ~ zeroOrMore(Pair) ~ ch('}') ~> ( scala.collection.immutable.Map(_:_*) )
  )
  def Pair = rule(
    Elem ~ Elem ~> ((_, _))
  )

  /**
    * COMMENT
    */
  def Comment = rule(
    ch(';') ~ zeroOrMore(!Newline ~ ANY) ~ (Newline | EOI)
  )


  /**
    * All Whitespaces
    */
  def SkipWS = rule( zeroOrMore(WS_D_C_NL) )

  def WS_D_C_NL = rule( WS_NL_CommaChar | Discard | Comment )

  def WS = rule( zeroOrMore(WSChar) )
  def Newline = rule { optional('\r') ~ '\n' }

  def ws(c: Char) = rule { c ~ WSChar }

  val QuoteBackSlash = CharPredicate("\"\\")

  val WSChar = CharPredicate(" \t")

  val WS_NL_CommaChar = CharPredicate(" \n\r\t,")

}
