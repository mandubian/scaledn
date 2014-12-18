package scaledn
package write

import  play.api.data.mapping._

import scaledn._

object Writes extends Writes

trait Writes extends play.api.data.mapping.DefaultWrites with LowWrites {

  import play.api.libs.functional.Monoid

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
  implicit val bigIntW = Write[BigInt, String]( l => l.toString + "M" )
  implicit val bigDecimalW = Write[BigDecimal, String]( l => l.toString + "N" )

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
  implicit val keywordW = Write[EDNKeyword, String]( s => s.toString )
  implicit def taggedW[A] = Write[EDNTagged[A], String]( s => s.toString )

  implicit def seqSW[A] = Write[Seq[A], String]( s => s.mkString("(", " ", ")") )
  implicit def listSW[A] = Write[List[A], String]( s => s.mkString("(", " ", ")") )
  implicit def vectorSW[A] = Write[Vector[A], String]( s => s.mkString("[", " ", "]") )
  implicit def setSW[A] = Write[Set[A], String]( s => s.mkString("#{", " ", "}") )
  implicit def mapSW[K, V](implicit wk: Write[K, String], wv: Write[V, String]) =
    Write[Map[K, V], String]{ s => s.map{ case (k,v) =>
      s"${wk.writes(k)} ${wv.writes(v)}" }.mkString("{", ",", "}")
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
    case t: EDNTagged[_] => taggedW.writes(t)
    case EDNNil => "nil"
    case s: List[_] => listSW.writes(s)
    case s: Vector[_] => vectorSW.writes(s)
    case s: Set[_] => setSW.writes(s)
    case s: Seq[_] => seqSW.writes(s)
    case s: Map[EDN @ unchecked, EDN @ unchecked] => mapSW[EDN, EDN](ednW, ednW).writes(s)

    case _ => throw new RuntimeException("$_ unsupported EDN type")
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

  import shapeless.{HList, Poly1, ::, HNil, Generic}

  implicit def writeHNil: Write[HNil, String] = Write { _ => "()" }

  implicit def writeHList1[H](implicit wh: Write[H, String]): Write[H :: HNil, String] =
    Write { hl => "(" + wh.writes(hl.head) + ")" }

  implicit def writeHList[H, HT <: HList](
    implicit
      wh: Write[H, String],
      wt: Write[HT, String],
      toTraversable: shapeless.ops.hlist.ToTraversable.Aux[H :: HT, List, Any]
    ): Write[H :: HT, String] =
    Write { hl =>
      hl.toList.map(ednW.writes(_)).mkString("(", " ", ")")
    }


}


trait LowWrites {
  import shapeless._
  import shapeless.labelled.FieldType
  import syntax.singleton._

  implicit def genWrite[P <: Product, HL <: HList](
    implicit gen: LabelledGeneric.Aux[P, HL], w: Write[HL, String]
  ): Write[P, String] =
    Write{ p =>
      w.writes(gen.to(p))
    }

  implicit def fieldType[K, V](implicit witness: Witness.Aux[K]) = Write[FieldType[K, V], String] { f =>
    witness.value.toString + " " + f.asInstanceOf[V].toString
  }
}