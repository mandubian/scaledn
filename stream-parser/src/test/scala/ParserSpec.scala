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

import org.scalatest._
import scala.util.{Try, Success, Failure}

import scalaz.stream.Process
import scalaz.stream.parsers._

import EDNToken._

class StreamParserSpec extends FlatSpec with Matchers with TryValues {
  "EDN lexing" should "lex a string" in {
    val output = Process("\"toto\"": _*).toSource pipe tokenize(EDNToken.Rules) stripW

    output.runLog.run should equal (Seq(EDNStr("toto")))
  }

  it should "lex a long" in {
    val output = Process("123": _*).toSource pipe tokenize(EDNToken.Rules) stripW

    output.runLog.run should equal (Seq(EDNLong(123)))
  }

  it should "lex a double" in {
    val output = Process("-123.345e-4": _*).toSource pipe tokenize(EDNToken.Rules, 2) stripW

    output.runLog.run should equal (Seq(EDNDouble(-123.345e-4)))
  }

  "EDN parsing" should "parse the fundamental values" in {
    parseEDN(Process("\"toto\"": _*).toSource).runLog.run should equal (Seq("toto"))
    parseEDN(Process("123": _*).toSource).runLog.run should equal (Seq(123L))
    parseEDN(Process("-123.345e-4": _*).toSource).runLog.run should equal (Seq(-123.345e-4))
  }
}