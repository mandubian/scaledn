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

import scalaz._
import scalaz.syntax.equal._
import scalaz.syntax.monad._
import scalaz.syntax.show._
import scalaz.std.anyVal._
import scalaz.concurrent.Task

import scalaz.stream._
import scalaz.stream.parsers._

import scala.util.matching.Regex

import Parser._

package object parser {

  def parseEDN(in: Process[Task, Char]): Process[Task, EDN] = 
      in.pipe(tokenize(EDNToken.Rules))
        .stripW
        .pipe(parse(SParser.root))
        .stripW
  /**
   * Somewhat-inefficiently (but usefully!) tokenizes an input stream of characers into
   * a stream of tokens given a set of regular expressions and mapping functions.  Note
   * that the resulting process will have memory usage which is linearly proportional to
   * the longest *invalid* substring, so…be careful.  There are better ways to implement
   * this function.  MUCH better ways.
   */
  def tokenize[T](
    rules: Map[Regex, PartialFunction[List[String], T]],
    whitespace: Option[Regex] = Some("""\s+""".r)
  ): Process1[Char, Char \/ T] = {
    import Process._

    def iseqAsCharSeq(seq: IndexedSeq[Char]): CharSequence = new CharSequence {
      def charAt(i: Int) = seq(i)
      def length = seq.length
      def subSequence(start: Int, end: Int) = iseqAsCharSeq(seq.slice(start, end))
      override def toString = seq.mkString
    }

    def attempt(buffer: CharSequence, requireIncomplete: Boolean): Option[T] = {
      println(s"buffer:$buffer")
      def matchBoth(pattern: Regex, pf: PartialFunction[List[String], T]): Boolean = {
        pattern findPrefixMatchOf buffer filter {
          !requireIncomplete || _.matched.length < buffer.length
        } map { t =>
          val v = t.subgroups
          println("SUB:"+v)
          v
        } collect pf isDefined
      }

      rules collectFirst {
        case (pattern, pf) if matchBoth(pattern, pf) =>
          pattern findPrefixMatchOf buffer map { _.subgroups } collect pf get      // I hate how we have to split this...
      }
    }

    /*
     * Buffer up characters until we get a prefix match PLUS one character that doesn't match (this
     * is to defeat early-completion of greedy matchers).  Once we get a prefix match that satisfies
     * a rule in the map, emit the resulting token and flush the buffer.  Any characters that aren't
     * matched by any rule are emitted.
     *
     * Note that the `tokenize` function should probably have a maxBuffer: Int parameter, or similar,
     * since it would be possible to DoS this function by simply feeding an extremely long unmatched
     * prefix.  Better yet, we should just knuckle-down and write a DFA compiler.  Would be a lot
     * simpler.
     */
    def inner(buffer: Vector[Char]): Process1[Char, Char \/ T] = {
      receive1Or[Char, Char \/ T](
        attempt(iseqAsCharSeq(buffer), false)
          .map { \/-(_) }
          .map (emit)
          .getOrElse (emitAll(buffer map { -\/(_) }))
      ){ c =>
        val buffer2 = buffer :+ c
        val csBuffer = iseqAsCharSeq(buffer2)

        val wsMatch = whitespace flatMap { _ findPrefixOf csBuffer }

        // if we matched prefix whitespace, move on with a clean buffer
        wsMatch map { prefix =>
          inner(buffer2 drop prefix.length)
        } getOrElse {
          attempt(csBuffer, true)
            .map { \/-(_) }
            .map { t => emit(t) ++ inner(Vector(c)) }
            .getOrElse (inner(buffer2))
        }
      }
    }

    inner(Vector.empty)
  }
}