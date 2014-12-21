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
 package object scaledn {

  /** This is a dummy alias to make you believe you manipulate 
    * a very special EDN type whereas it is just an Any restricted
    * by the EDN specified parsing to those types:
    *
    * - Long (64bits)       12345
    * - Double (64 bits)    123.45
    * - BigInt              12345M
    * - BigDecimal          123.45N
    * - String              "foobar"
    * - Characters          \c \newline \return \space \tab \\ \u0308 etc...
    * - EDN Symbol          foo/bar
    * - EDN Keyword         :foo/bar
    * - EDN Nil             nil
    * - Tagged vaues        #foo/bar value
    * - heterogenous list   (1 true "toto")
    * - heterogenous vector [1 true "toto"]
    * - heterogenous set    #{1 true "toto"}
    * - heterogenous map    {1 "toto", 1.234 "toto"}
    *
    * For more info, go to the [EDN format site](https://github.com/edn-format/edn)
    */
  type EDN = Any

  /** Another alias to be used with Path writes (is it useful?)
    */
  type EDNMap = Map[String, EDN]
}