SCALEDN, [EDN](https://github.com/edn-format/edn) Scala API
===============

A Scala [EDN](https://github.com/edn-format/edn) parser/serializer/validator based on :

- [Parboiled2](https://github.com/sirthias/parboiled2),
- [Shapeless](https://github.com/milessabin/shapeless),
- [Generic Validation](https://github.com/jto/validation)
- Scala Macros

> It works only in Scala 2.11.x


## Why EDN?...

> Because Json is not enough & quite limitating

EDN is described as an _extensible data notation_ specified (not really standardized) [there](https://github.com/edn-format/edn). Clojure & Datalog used in Datomic are supersets of EDN.

EDN allows much more things than Json while keeping the same simplicity.

Here are the main points making EDN great to represent & exchange Data

<br/>
### EDN manages number types far better than Json

For Json, all numbers (floating or integer, exponential or not) are all considered in the same way so numbers can only be mapped to the biggest number format: `BigDecimal`. It is really bad in terms of semantics and performance.

In EDN, numbers can be :

- 64bits integer aka `Long` in Scala:

```
12345
```
- 64bits floating point numbers & exponentials aka `Double` in Scala:

```
123.45e-9
```
- Natural Integers aka `BigInt` in Scala:

```
1234567891234N
```
- Exact Floating Number aka `BigDecimal` in Scala:

```
123.4578972345M
```


<br/>
### EDN knows much more about collections

Collections in Json are just:

- lists of heterogenous json values
- maps of key strings and json values.

In EDN, you can have:

- heterogenous lists

```
(1 true "toto)
```
- heterogenous vectors/arrays
```
[1 true "toto]
```
- heterogenous sets
```
#{1 true "toto}
```
- heterogenous maps with heterogenous keys & values
```
{1 "toto", "foo" 2}
```

<br/>
### EDN accepts characters & unicode

Json doesn't know about characters outside strings.

EDN can manage chars:

```
// simple char
\c

// special chars
\newline
\return
\space
\tag
\\

// unicode
\u0308
```

<br/>
### EDN accepts comments & discarded values

There are special syntaxes:

- comments are lines starting with `;`
- values starting with `#_` are parsed but discarded

<br/>
### EDN knows about symbols & keywords

These are notions that don't exist in Json.

Symbols can reference anything external or internal that you want to identify. A `Symbol` can have a namespace such as `foo/bar`.

Keywords are unique identifiers or enumerated values that can be reused in your data structure. A `Keyword` is just a symbol preceded by a `:` such as :foo/bar.

<br/>
### EDN is extensible using tags

EDN is an extensible format using tags starting with `#` such as:

```
#foo/bar value
```

When parsing EDN format, the parser should provide tag handlers that can be applied when a tag is discovered. In this way, you can extend default format with your own formats.

EDN specifies 2 tag handlers by default:

- `#inst "1985-04-12T23:20:50.52Z"` for RFC-3339 instants
- `#uuid "f81d4fae-7dec-11d0-a765-00a0c91e6bf6"` for UUID

<br/>
### EDN has no root node & can be streamed

Json is defined to have a root `map` node: `{ key : value }` or `[ ... ]`.

Json can't accept single values outside of this. So Json isn't really meant to be streamed as you need to find closing tags to finish parsing a value.

EDN doesn't require this and can consist in multiple heterogenous values:

```1 123.45 "toto" true nil (1 2 3)```

As a consequence, EDN can be used to stream your data structures.

<br/>
### Conclusion: EDN should be preferred to Json

All of these points make EDN a far better & stricter & more evolutive notation to represent data structures than Json. It can be used in the same way as Json but could make a far better RPC string format than Json.

I still wonder why Json has become the de-facto standard except for the reason that the _not so serious_ Javascript language parses it natively and because people were so sick about XML that they would have accepted anything changing their daily life.

But JS could also parse EDN without any problem and all more robust & typed backend languages would earn a lot from using EDN instead of JSON for their interfaces.

EDN could be used in REST API & also for streaming API.
That's exactly why, I wanted to provide a complete Scala API for EDN to test this idea a bit further.

<br/>
<br/>
## Scaledn insight

<br/>
### Runtime Parsing 

Scaledn can be used to parse the EDN string or arrays of chars received by your API.

All types described in EDN format are isomorphic to Scala types so I've decided to skip the complete AST wrapping those types and directly parse to Scala types.

- `"foobar"` is parsed to `String`
- `123` is parsed to `Long`
- `(1 2 3)` is parsed to `List[Long]`
- `(1 "toto" 3)` is parsed to `List[Any]`
- `{"toto" 1 "tata" 2}` is parsed to `Map[String, Long]`
- `{1 "toto" 2 "tata"}` is parsed to `Map[Long, String]`
- `{1 "toto" true 3}` is parsed to `Map[Any, Any]`
- etc...

The parser (based on [Parboiled2](https://github.com/sirthias/parboiled2)) provides 2 main functions:

```scala
import scaledn._
import parser._

// parses only the first EDN value discovered in the String input
def parseEDN(in: ParserInput): Try[EDN] = ...

// parses all EDN values discovered in the String input
def parseEDNs(in: ParserInput): Try[Seq[EDN]] = ...
```

If you look in common package, you'll see that `EDN` is just an alias for `Any` ;)


Here is how you can use it:

```scala
import scaledn._
import parser._

// Single Value
parseEDN("""{1 "foo", "bar" 1.234M, :foo/bar [1,2,3]} #_foo/bar :bar/foo""") match {
  case Success(t) => \/-(t)
  case Failure(f : org.parboiled2.ParseError) => -\/(parser.formatError(f))
}

// Multiple Value
parseEDNs("""{1 "foo", "bar" 1.234M, :foo/bar [1,2,3]} :bar/foo""").success.value should be (
  Vector(
    Map(
      1L -> "foo",
      "bar" -> BigDecimal("1.234"),
      EDNKeyword(EDNSymbol("foo/bar", Some("foo"))) -> Vector(1, 2, 3)
    ),
    EDNKeyword(EDNSymbol("bar/foo", Some("bar")))
  )
))
```

> Some people will think `Any` is a bit too large and I agree but it's quite practical to use. Moreover, using validation explained a bit later, you can parse your EDN and then map it to a stronger typed scala structure and then `Any` disappears.

<br/>
## Compile-time parsing with Macros

When you use static EDN structures in your Scala code, you can write them in their string format and _scaledn_ can parse them at compile-time using Scala macros and thus prevent a lot of errors you can encounter in dynamic languages.

The macro mechanism is based on quasiquotes & whitebox macro contexts which allow to infer types of your parsed EDN structures at compile-time. For example:

```scala
> val s:Long = EDN("\"toto\"")

[error]  found   : String("toto")
[error]  required: Long
[error]     val e: Long = EDN("\"toto\"")

```

Whooohooo magic :)


<br/>
### Classic Scala types

Here is how you can use it:

```scala
import scaledn._
import macros._

// All types are just for info and can be omitted below, the macro infers them quite well
val e: String = EDN("\"toto\"")

val bt: Boolean = EDN("true")

val bf: Boolean = EDN("false")

val l: Long = EDN("123")

val d: Double = EDN("123.456")

val bi: BigInt = EDN("123M")

val bd: BigDecimal = EDN("123.456N")

val s: EDNSymbol = EDN("foo/bar")

val kw: EDNKeyword = EDN(":foo/bar")

// Homogenous collection inferred as Vecto[String]
val vector: Vector[String] = EDN("""["tata" "toto" "tutu"]""")

// multiple heterogenous values inferred as Seq[Any]
val s = EDNs("""(1 2 3) "toto" [true false] :foo/bar""")
// note the small s at the end of EDN to inform the macro there are several values
```


### Shapeless heterogenous collections

EDN allows to manipulate heterogenous collections. In Scala, when one thinks _heterogenous collection_, one thinks [Shapeless](https://github.com/milessabin/shapeless). Scaledn macros can parse & map your EDN stringified structures to Scala strongly typed structures.


```scala
import scaledn._
import macros._

import shapeless.{HNil, ::}
import shapeless.record._
import shapeless.syntax.singleton._

// Heterogenous list
val s = EDNH("""(1 "toto" true)""")
s should equal (1L :: "toto" :: true :: HNil)

// Heterogenous Map/Record
val s3 = EDNH("""{1 "toto" true 1.234 "foo" (1 2 3)}""")
s3 should equal (
  1L ->> "toto" ::
  true ->> 1.234 ::
  "foo" ->> List(1L, 2L, 3L) ::
  HNil
)
```

> please note the `H` in `EDNH` for heterogenous

> I must say using these macros, it might be even simpler to write Shapeless hlists or records than using scala API ;)

<br/>
### Macro API

Scaledn provides different macros depending on the depth of introspection you require in your collection with respect to heterogeneity.

Have a look directly at [Macro API](https://github.com/mandubian/scaledn/blob/master/macros/src/main/scala/macros.scala)

<br/>
### Mixing macro with Scala string interpolation

Following ideas implemented by Daniel James in [Datomisca](http://pellucidanalytics.github.io/datomisca/), scaledn proposes to use String interpolation mixed with parsing macro such as:

```scala
import scaledn._
import macros._

import shapeless.{HNil, ::}

val l = 123L
val s = List("foo", "bar")

val r: Long = EDN(s"$l")

val r1: Seq[Any] = EDN(s"($l $s)")
val r2: Long :: List[String] :: HNil = EDNH(s"($l $s)")
```

Nothing to add, macros are cool sometimes :)

<br/>
<br/>
## Runtime validation of EDN to Scala

When writing REST or external API, the received data can never be trusted before being validated. So, you generally try to validate what is received and map it to a strong-typed structures. For example:

```scala
// parse the received string input
parseEDN("""{ 1 "toto" 2 "tata" 3 "tutu" }""")
// then validate it to a Scala type
  .map(validate[Map[Long, String]])
  .success.value should be (
    play.api.data.mapping.Success(Map(
      1L -> "toto",
      2L -> "tata",
      3L -> "tutu"
    ))
  )
```

The validation API is the following:

```scala
import scaledn._
import validate._

def validate[T](edn: EDN)(implicit r: RuleLike[EDN, T]): Validation[EDN, T] = r.validate(edn)
```

Scaledn validation is based on [Generic Validation API](https://github.com/jto/validation) developed by my [MFGLabs](http://www.mfglabs.com)'s colleague & friend [Julien Tournay](https://github.com/jto). This API was developed for Play Framework & Typesafe last year to generalize Json validation API to all data formats. But it will never be integrated in Play as Typesafe considers it to be too pure Scala & pure FP-oriented. Yet, we use this API in production at [MFGLabs](http://www.mfglabs.com) and maintain/extend it ourselves.

As explained before, Scaledn parser parses EDN values directly to Scala types as they are bijective so validation is often just a runtime cast and not very interesting in general.

What's much more interesting is to validate to Shapeless HList, Records and even more interesting to CaseClasses & Tuples based on Shapeless fantastic auto-generated Generic macros.

Let's take a few examples to show the power of this feature:

```scala
import scaledn._
import validate._

import play.api.data.mapping._
import shapeless.{HNil, ::}

case class CP(cp: Int)
case class Address(street: String, cp: CP)
case class Person(name: String, age: Int, addr: Address)
// Remark that NO implicits must be declared on our case classes

// HLISTS
parseEDN("""(1 "toto" true nil)""").map(
  validate[Long :: String :: Boolean :: EDNNil.type :: HNil]
).success.value should be (
  Success(1L :: "toto" :: true :: EDNNil :: HNil)
)

// TUPLES
parseEDN("""("toto" 34 {"street" "chboing", "cp" {"cp" 75009}})""").map(
  validate[Tuple3[String, Int, Address]]
).success.value should be (
  Success(("toto", 34, Address("chboing", CP(75009))))
)

// CASECLASSES
parseEDN("""("toto" 34 ("chboing" (75009)))""").map(
  validate[Person]
).success.value should be (
  Success(Person("toto", 34, Address("chboing", CP(75009))))
)

parseEDN("""{"name" "toto", "age" 34, "addr" {"street" "chboing", "cp" {"cp" 75009}}}""").map(
  validate[Person]
).success.value should be (
  Success(Person("toto", 34, Address("chboing", CP(75009))))
)

```

> I think here you can see the power of this validation feature without writing any boilerplate...


<br/>
<br/>
## Serializing Scala to EDN

Using [Generic Validation API](https://github.com/jto/validation), you can also write scala structures to any other data format.

```scala
import scaledn._
import write._

toEDNString("toto") should equal ("\"toto\"")
toEDNString(List(1, 2, 3)) should equal ("""(1 2 3)""")
```

The write API is the following:

```scala
import scaledn._
import write._

def toEDNString[I](i: I)(implicit w: WriteLike[I, String]): String = w.writes(i) 
```

Once again, what's more interesting is using shapeless & caseclasses & tuples.

```scala
import scaledn._
import write._

import shapeless.{HNil, ::}

// HLIST
toEDNString(1 :: true :: List(1L, 2L, 3L) :: HNil) should equal ("""(1 true (1 2 3))""")

// TUPLE
toEDNString((23, true)) should equal ("""(23 true)""")

// CASE CLASS
case class Address(street: String, cp: Int)
case class Person(name: String, age: Int, addr: Address)
// Remark that NO implicits must be declared on our case classes

toEDNString(Person("toto", 34, Address("chboing", 75009))) should equal (
  """{"name" "toto", "age" 34, "addr" {"street" "chboing", "cp" 75009}}"""
)
```


## TODO

This project is a first draft so it requires a bit more work.

Here are a few points to work on:

- patch remaining glitches/bugs
- write more tests for all cases
- study streamed parser asap
- write sample apps

Don't hesitate to test, find bugs, contribute, give remarks, ideas...

Have fun in EDN world..

