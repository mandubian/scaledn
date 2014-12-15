package scaledn

package object write extends Writes {
  import  play.api.data.mapping.Write

  def toEDNString[I](i: I)(implicit w: Write[I, String]): String = w.writes(i) 
}


package object validate {
  import  play.api.data.mapping.{Rule, Validation}

  def validate[T](edn: EDN)(implicit r: Rule[EDN, T]): Validation[EDN, T] = r.validate(edn)
}