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

  final case class EDNStr(str: String)      extends EDNToken        // "foo"
  final case class EDNLong(value: Long)     extends EDNToken        // 3
  final case class EDNDouble(value: Double) extends EDNToken    // 3.14
  final case class EDNExp(value: String)    extends EDNToken    // 3.14

  case object True  extends EDNToken    // true
  case object False extends EDNToken   // false

  val stringLiteral = ("\""+"""(([^"\p{Cntrl}\\]|\\[\\'"bfnrt]|\\u[a-fA-F0-9]{4})*)"""+"\"").r

  val floatingPointNumber = """(-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?)""".r

  val decimalNumber = """(\d+(\.\d*)?|\d*\.\d+)""".r

  val naturalNumber = """(\d+)""".r

  val exponential = """([eE][+-]?\d+)""".r

  val Rules: Map[Regex, PartialFunction[List[String], EDNToken]] = Map(
    stringLiteral       -> { case body :: _   => EDNStr(canonicalizeStr(body)) },
    naturalNumber       -> { case body :: _   => EDNLong(body.toLong) },
    floatingPointNumber -> { case body :: _   => EDNDouble(body.toDouble) },
    exponential         -> { case body :: _   => EDNExp(body) }
    // """\{""".r  -> { case Nil => LBrace },
    // """\}""".r  -> { case Nil => RBrace },

    // """\[""".r  -> { case Nil => LBracket },
    // """\]""".r  -> { case Nil => RBracket },

    // """,""".r   -> { case Nil => Comma },


    // "true".r    -> { case Nil => True },
    // "false".r   -> { case Nil => False }
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

