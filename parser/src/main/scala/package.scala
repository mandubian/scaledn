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


package object parser {
  import org.parboiled2.ParserInput
  import scala.util.{Try, Success, Failure}

  /** parses a string input to EDN but keep only first element
    *
    * {{{
    * import scaledn.parser._
    *
    * parseEDN("""(1, "foo", :foo/bar)""") match {
    *   case Success(t) => \/-(t)
    *   case Failure(f : org.parboiled2.ParseError) => -\/(parser.formatError(f))
    * }
    * }}}
    *
    * @param in the string input
    * @return a single EDN or the parsing error
    */
  def parseEDN(in: ParserInput): Try[EDN] = EDNParser(in).Root.run().map(_.head)

  /** parses a string input to EDN and keep all elements
    *
    * {{{
    * import scaledn.parser._
    *
    * parseEDNs("""(1, "foo", :foo/bar) [1 2 3]""") match {
    *   case Success(t) => \/-(t)
    *   case Failure(f : org.parboiled2.ParseError) => -\/(parser.formatError(f))
    * }}}
    *
    * [[https://github.com/sirthias/parboiled2/blob/master/parboiled-core/src/main/scala/org/parboiled2/ParserInput.scala org.parboiled2.ParserInput]] provides implicit conversions from [[java.lang.String]] or
    * [[http://docs.oracle.com/javase/6/docs/api/java/lang/String.html java.lang.Array[Byte]]]
    *
    * @param in the string input
    * @return a Seq[EDN] or the parsing error
    */
  def parseEDNs(in: ParserInput): Try[Seq[EDN]] = EDNParser(in).Root.run()

}