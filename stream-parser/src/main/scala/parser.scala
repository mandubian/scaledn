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
package stream
package parser

import scalaz._
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import scalaz.syntax.show._
import scalaz.std.anyVal._

import scalaz.stream._
import scalaz.stream.parsers._

import scala.util.matching.Regex

import Parser._


/** copied from D.Spiewak Json sample */
sealed trait EDNToken {
  final val as: EDNToken = this     // lightweight ascription
}

object EDNToken {
  case object LBrace extends EDNToken    // {
  case object RBrace extends EDNToken    // }

  case object LBracket extends EDNToken  // [
  case object RBracket extends EDNToken  // ]

  case object Comma extends EDNToken     // ,

  final case class EDNStr(str: String)        extends EDNToken    // "foo"
  final case class EDNLong(value: Long)       extends EDNToken    // 3
  final case class EDNNatural(value: BigInt)  extends EDNToken    // 3872348739479847397397392739479377
  final case class EDNDouble(value: Double)   extends EDNToken    // 3.14
  final case class EDNReal(value: BigDecimal) extends EDNToken    // 1.12356346238763876283746837246238

  case object True  extends EDNToken    // true
  case object False extends EDNToken   // false

  val stringLiteral = ("\""+"""(([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*)"""+"\"").r

  val floatingPointNumber = """((-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?))(N?)""".r

  val exponential = """([eE][+-]?\d+)""".r

  val naturalNumber = """(-?\d+)(M?)""".r

  val Rules: Seq[(Regex, PartialFunction[List[String], EDNToken])] = Seq(
    stringLiteral       ->  { case body :: _  =>  EDNStr(canonicalizeStr(body)) },

    naturalNumber       ->  { case body :: t  =>  t.last match {
                                                    case "M" => EDNNatural(BigInt(body))
                                                    case _   => EDNLong(body.toLong) }},

    floatingPointNumber ->  { case body :: t  =>  t.last match {
                                                    case "N" => EDNReal(BigDecimal(body))
                                                    case _   => EDNDouble(body.toDouble) }},

    "true".r            ->  { case Nil        =>  True  },
    "false".r           ->  { case Nil        =>  False }

    // """\{""".r  -> { case Nil => LBrace },
    // """\}""".r  -> { case Nil => RBrace },

    // """\[""".r  -> { case Nil => LBracket },
    // """\]""".r  -> { case Nil => RBracket },

    // """,""".r   -> { case Nil => Comma },


  )

  private def canonicalizeStr(body: String): String = {
    val (_, back) = body.foldLeft((false, "")) {
      case ((true, acc), c) => (false, acc + c)
      case ((false, acc), '\\') => (true, acc)
      case ((false, acc), c) => (false, acc + c)
    }

    back
  }

  implicit val eq: Equal[EDNToken] = Equal.equalA[EDNToken]
  implicit val show: Show[EDNToken] = Show.showA[EDNToken]

  def tokenizeOptim(
    maxMatchDepth: Int = 1,
    whitespace: Option[Regex] = Some("""\s+""".r)
  ): Process1[Char, Char \/ EDNToken] = {
    import Process._

    def attempt(buffer: CharSequence, requireIncomplete: Boolean): Option[EDNToken] = {
      // glory to ugly mutable code ;)
      var opt: Option[EDNToken] = None
      val it = Rules.iterator

      while(it.hasNext && opt.isEmpty) {
        val (pattern, pf) = it.next
        pattern
          .findPrefixMatchOf(buffer)
          .filter(!requireIncomplete || _.matched.length < buffer.length)
          .foreach { m =>
            val subs = m.subgroups
            println(s"found subs:$subs")
            if(pf isDefinedAt subs) opt = Some(pf(subs))
          }
      }

      opt
    }

    def inner(buffer: Vector[Char], depth: Int): Process1[Char, Char \/ EDNToken] = {
      println(s"inner buffer:$buffer depth:$depth")
      receive1Or[Char, Char \/ EDNToken](
        attempt(iseqAsCharSeq(buffer), false)
          .map { \/-(_) }
          .map (emit)
          .getOrElse (emitAll(buffer map { -\/(_) }))
      ){ c =>
        
        c match {
          case '"'  =>  tokenize1(
                          stringLiteral,
                          { case body :: _  =>  EDNStr(canonicalizeStr(body)) },
                          Vector('"')
                        )
          // case '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '-' | '+' => Double | Long
          // case ':'  => Keyword
          // case '('  => List
          // case '{'  => Map
          // case '['  => Vector
          // case '#'  => Set | Tagged
          // case 't'  => True | Symbol
          // case 'f'  => False | Symbol
          // case 'n'  => Nil | Symbol
          // case '\\' => Char
          // case _    => Symbol
        }
      }
    }

    inner(Vector.empty, maxMatchDepth)

  }
}


object SParser {
  import EDNToken._

  lazy val root: Parser[EDNToken, EDN] = (
      strValue
    | longValue
    | doubleValue
  )

  lazy val strValue: Parser[EDNToken, EDN] =
    pattern { case EDNStr(body) => body }

  lazy val longValue: Parser[EDNToken, EDN] =
    pattern { case EDNLong(body) => body }

  lazy val doubleValue: Parser[EDNToken, EDN] =
    pattern { case EDNDouble(body) => body }

}

