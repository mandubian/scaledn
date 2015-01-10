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
package write

import  play.api.data.mapping._
import shapeless.DepFn1

import scaledn._

object Writes extends Writes

trait Writes extends LowWrites {

  import play.api.libs.functional.Monoid

  trait Strategy[A] extends DepFn1[A]

  object Strategy {
    type Aux[A, O] = Strategy[A]{ type Out = O }
  }

  implicit def productStrategy[P, HL <: HList](
    cc: HasProductGeneric[P],
    not: P <:!< EDNValue,
    gen: LabelledGeneric.Aux[P, HL]
  ): Strategy.Aux[P, EDNTagged[HL]] = new Strategy[P] {
    type Out = EDNTagged[HL]

    def apply(p: P) = EDNTagged()
  }

  implicit def jsonMonoid = new Monoid[EDNMap] {
    def append(a1: EDNMap, a2: EDNMap) = a1 ++ a2
    def identity = Map.empty[String, EDN]
  }

  implicit val stringW = Write[String, String]( str => "\"" + str + "\"" )
  implicit val booleanW = Write[Boolean, String]( l => if(l) "true" else "false" )
  implicit val longW = Write[Long, String]( l => l.toString )
  implicit val shortW = Write[Short, String]( l => l.toString )
  implicit val intW = Write[Int, String]( l => l.toString )
  implicit val floatW = Write[Float, String]( l => l.toString )
  implicit val doubleW = Write[Double, String]( l => l.toString )
  implicit val bigIntW = Write[BigInt, String]( l => l.toString + "N" )
  implicit val bigDecimalW = Write[BigDecimal, String]( l => l.toString + "M" )

  implicit val charW = Write[Char, String]{
    case '\n' => """\newline"""
    case '\r' => """\return"""
    case ' '  => """\space"""
    case '\t' => """\tab"""
    case '\\' => "\\"
    case c if c.isLetterOrDigit => "\\"+c
    case unicode => "\\u%04X".format(unicode.toInt)
  }

  implicit val symbolW = Write[EDNSymbol, String]( s => s.toString )
  implicit val nilW = Write[EDNNil.type, String]( s => "nil" )
  implicit val keywordW = Write[EDNKeyword, String]( s => ":" + symbolW.writes(s.value) )

  implicit def taggedW[A](implicit w: Write[A, String]) =
    Write[EDNTagged[A], String]( t => s"#${t.tag} ${w.writes(t.value)}" )

  implicit def seqSW[A](implicit w: Write[A, String]) = Write[Seq[A], String]( s => s.map(w.writes).mkString("(", " ", ")") )
  implicit def listSW[A](implicit w: Write[A, String]) = Write[List[A], String]( s => s.map(w.writes).mkString("(", " ", ")") )
  implicit def vectorSW[A](implicit w: Write[A, String]) = Write[Vector[A], String]( s => s.map(w.writes).mkString("[", " ", "]") )
  implicit def setSW[A](implicit w: Write[A, String]) = Write[Set[A], String]( s => s.map(w.writes).mkString("#{", " ", "}") )
  implicit def mapSW[K, V](implicit wk: Write[K, String], wv: Write[V, String]) =
    Write[Map[K, V], String]{ s => s.map{ case (k,v) =>
      s"${wk.writes(k)} ${wv.writes(v)}" }.mkString("{", ", ", "}")
    }

  val mapSWOpt = Write[Map[EDN, EDN], String]{ s => 
    s.map{ case (k,v) =>
      v match {
        case o: Option[EDN] => o match {
          case None    => ""
          case Some(v) => s"${ednW.writes(k)} ${ednW.writes(v)}"
        }
        case v => s"${ednW.writes(k)} ${ednW.writes(v)}"
      }
    }.filterNot(_.isEmpty).mkString("{", ", ", "}")
  }

  def optW[A](implicit w: Write[A, String]) = Write[Option[A], String]{
    case None    => ""
    case Some(a) => w.writes(a)
  }

  implicit def ednW: Write[EDN, String] = Write[EDN, String]{
    case s: String => stringW.writes(s)
    case b: Boolean => booleanW.writes(b)
    case l: Long => longW.writes(l)
    case s: Short => shortW.writes(s)
    case i: Int => intW.writes(i)
    case f: Float => floatW.writes(f)
    case d: Double => doubleW.writes(d)
    case b: BigInt => bigIntW.writes(b)
    case b: BigDecimal => bigDecimalW.writes(b)
    case c: Char => charW.writes(c)
    case s: EDNSymbol => symbolW.writes(s)
    case k: EDNKeyword => keywordW.writes(k)
    case t: EDNTagged[EDN @unchecked] => taggedW(ednW).writes(t)
    case EDNNil => "nil"
    case s: List[EDN] => listSW(ednW).writes(s)
    case s: Vector[EDN] => vectorSW(ednW).writes(s)
    case s: Set[EDN @unchecked] => setSW(ednW).writes(s)
    case s: Seq[EDN] => seqSW(ednW).writes(s)
    case s: Map[EDN @ unchecked, EDN @ unchecked] => mapSWOpt.writes(s)
    case s: Option[EDN] => optW[EDN].writes(s)

    case s => throw new RuntimeException(s"$s unsupported EDN type")
  }

  implicit def write2Path[I](path: Path): Write[I, Map[String, Any]] =
    Write { i =>
      path match {
        case Path(KeyPathNode(x) :: _) \: _ =>
          val ps = path.path.reverse
          val KeyPathNode(k) = ps.head
          val o = Map[String, EDN](k -> i)
          ps.tail.foldLeft(o){
            case (os, KeyPathNode(k)) => Map[String, EDN](k -> os)
            case _ => throw new RuntimeException(s"path $path is not a path to a Map")
          }
        case _ => throw new RuntimeException(s"path $path is not a path to a Map") // XXX: should be a compile time error
      }
    }


  implicit def customTagged[I, J](implicit wi:Write[I, EDNTagged[J]], wj:Write[J, String]): Write[I, String] = 
    Write[I, String]{ i =>
      taggedW(wj).writes(wi.writes(i))
    }


}

trait LowWrites extends SuperLowWrites {

  import shapeless._
  import shapeless.labelled.FieldType
  import syntax.singleton._
  import tag.@@
  import shapeless.labelled.FieldType
  import shapeless.ops.hlist.IsHCons
  import syntax.singleton._
  import shapeless.ops.record.Selector
  import record._

  // import shapelessext._


  implicit def writeHNil: Write[HNil, String] = Write { _ => "()" }

  implicit def writeHList[H, HT <: HList](
    implicit
      wh: Write[H, String],
      wt: SeqWrite[HT, String]
    ): Write[H :: HT, String] =
    Write { case h :: t =>
      (wh.writes(h) +: wt.writes(t)).filterNot(_.isEmpty).mkString("(", " ", ")")
    }

  implicit def genWriteTuple[P, HL <: HList, HH , HT <: HList](
    implicit
      tuple: IsTuple[P],
      gen: Generic.Aux[P, HL],
      c: IsHCons.Aux[HL, HH, HT],
      wh: Write[HH, String],
      wt: SeqWrite[HT, String]
  ): Write[P, String] =
    Write{ p =>
      val t = gen.to(p)
      (wh.writes(t.head) +: wt.writes(t.tail)).filterNot(_.isEmpty).mkString("[", " ", "]")
    }

  implicit def subGenWrite[H, HT <: HList, K, V](
    implicit
      un: Unpack2[H, FieldType, K, V],
      wh: Write[FieldType[K, V], String],
      wt: SeqWrite[HT, String]
  ): SeqWrite[H :: HT, String] =
    SeqWrite{ case h :: t =>
      wh.writes(h.asInstanceOf[FieldType[K, V]]) +: wt.writes(t)
    }

  implicit def fieldTypeWO[K <: Symbol, V](implicit witness: Witness.Aux[K], wv: Write[V, String]) =
    Write[FieldType[K, Option[V]], String] { f =>
      f map { v => "\"" + witness.value.name + "\"" + " " + wv.writes(v) } getOrElse ""
    }

}

trait SuperLowWrites extends play.api.data.mapping.DefaultWrites {
  import shapeless._
  import shapeless.labelled.FieldType
  import shapeless.ops.hlist.IsHCons
  import syntax.singleton._
  import shapeless.ops.record.Selector
  import record._

  implicit def isoStrategy[I] = new Strategy[I] {
    type Out = I

    def apply(i: I) = i
  }
  
  implicit def genWriteCaseClass[P, K, V, F, HL <: HList, HT <: HList](
    implicit
      cc: HasProductGeneric[P],
      not: P <:!< EDNValue,
      gen: LabelledGeneric.Aux[P, HL],
      c: IsHCons.Aux[HL, F, HT],
      un: Unpack2[F, FieldType, K, V],
      wh: Write[FieldType[K, V], String],
      wt: SeqWrite[HT, String],
      witness: Witness.Aux[K],
      selector : Selector.Aux[HL, K, V]
  ): Write[P, String] =
    Write{ p =>
      val t = gen.to(p)
      //(wh.writes(t.fieldAt(witness)(selector)) +: wt.writes(t.tail)).filterNot(_.isEmpty).mkString("{", ", ", "}")
      (wh.writes(labelled.field[witness.T](selector(t))) +: wt.writes(t.tail)).filterNot(_.isEmpty).mkString("{", ", ", "}")
    }

  implicit def fieldTypeW[K <: Symbol, V](implicit witness: Witness.Aux[K], wv: Write[V, String]) =
    Write[FieldType[K, V], String] { f =>
      "\"" + witness.value.name + "\"" + " " + wv.writes(f)
    }

  trait SeqWrite[I, O] {
    def writes(i: I): Seq[O]
  }
  object SeqWrite{
    def apply[I, O](w: I => Seq[O]): SeqWrite[I, O] = new SeqWrite[I, O] {
      def writes(i: I) = w(i)
    }
  }

  // implicit def seqWrite[Tmplicit w: Write[T, String]): SeqWrite[T, String] = SeqWrite[T, String]{ s => Seq(w.writes(s)) }
  // implicit def scalaSymbolW[K <: Symbol] = SeqWrite[K, String]{ s => Seq("\"" + s.name + "\"") }
  // implicit def scalaSymbolTaggedW[T] = SeqWrite[Symbol @@ T, String]{ s => scalaSymbolW.writes(s) }

  implicit def subHNilW: SeqWrite[HNil, String] = SeqWrite { _ => Seq() }

  implicit def subHListW[H, HT <: HList](
    implicit
      wh: Write[H, String],
      wt: SeqWrite[HT, String]
  ): SeqWrite[H :: HT, String] =
    SeqWrite{ case h :: t =>
      wh.writes(h) +: wt.writes(t)
    }

}



