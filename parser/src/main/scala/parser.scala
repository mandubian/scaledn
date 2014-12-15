package scaledn
package parser

import org.parboiled2._
import scala.annotation.switch
import org.joda.time.{DateTime, DateTimeZone}

object EDNParser {
  def apply(input: ParserInput) = new EDNParser(input)
}

class EDNParser(val input: ParserInput) extends Parser with StringBuilding {
  import CharPredicate.{Digit, Digit19, HexDigit, Alpha, AlphaNum}

  implicit def wspStr(s: String): Rule0 = rule( str(s) ~ WS )

  def Root: Rule1[Seq[Any]] = rule( oneOrMore(Elem) ~ EOI )

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

  def Nil = rule { "nil" ~ push(EDNNil)}

  def True = rule { "true" ~ push(true) }
  def False = rule { "false" ~ push(false) }

  def Boolean = rule { True | False }

  /**
    * INTEGER
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
    * DOUBLE
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
    * STRING
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
  def Keyword = rule( ':' ~ !CharPredicate(":/") ~ Symbol ~> (EDNKeyword(_)) )

  /**
    * SYMBOL
    */
  def Symbol = rule ( 
    !CharPredicate(":#;") ~
    clearSB() ~
    optional(SymbolNameSpace ~ push(sb.toString)) ~ clearSB() ~
    SymbolString ~ push(sb.toString) ~> { (ns, value) =>
      EDNSymbol(ns.map(_+"/").getOrElse("") + value, ns)
    }
  )

  def SymbolNameSpace = rule(SymbolString ~ "/")
  def SymbolString = rule(
      SymbolSpecial
    | SymFirstChars ~ appendSB() ~ zeroOrMore(SymChars ~ appendSB())
  )
  def SymbolSpecial = rule(
    SymbolSpecialStart ~ zeroOrMore(SymChars ~ appendSB())
  )
  def SymbolSpecialStart = rule(
    capture(SymSpecialFirstChars ~ !Digit) ~> ((s:String) => appendSB(s)) 
  )
  def SymFirstChars = rule(Alpha | SymNonAlphaChars)
  def SymChars = rule(AlphaNum | SymNonAlphaChars | SymSpecialFirstChars)
  // non alpha chars without +-.
  val SymNonAlphaChars = CharPredicate("*!_?$%&=<>:#")
  val SymSpecialFirstChars = CharPredicate("+-.")

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
  def unknownTags = rule(Symbol ~ WS ~ Elem ~> ( EDNTagged(_, _) ))

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
    str("#{") ~ zeroOrMore(Elem) ~ ch('}') ~> ( scala.collection.immutable.Set(_:_*) )
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

  def SkipWS = rule( zeroOrMore(WS_D_C_NL) )

  def WS_D_C_NL = rule( WS_NL_CommaChar | Discard | Comment )

  def WS = rule( zeroOrMore(WSChar) )
  def Newline = rule { optional('\r') ~ '\n' }

  def ws(c: Char) = rule { c ~ WSChar }

  val QuoteBackSlash = CharPredicate("\"\\")

  val WSChar = CharPredicate(" \t")

  val WS_NL_CommaChar = CharPredicate(" \n\r\t,")

}
