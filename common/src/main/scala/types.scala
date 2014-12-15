package scaledn

// import shapeless.Generic

sealed trait EDNValue

case class  EDNSymbol(value: String, namespace: Option[String] = None) extends EDNValue {
  override def toString = value
}
case class  EDNKeyword(value: EDNSymbol) extends EDNValue {
  override def toString = s":$value"
}

case class  EDNTagged[A](tag: EDNSymbol, value: A) extends EDNValue {
  override def toString = s"#$tag ${value.toString}"
}

case object EDNNil extends EDNValue {
  override def toString = "nil"
}

case class  EDNBoolean(value: Boolean) extends EDNValue
case class  EDNString(value: String) extends EDNValue
case class  EDNChar(value: Char) extends EDNValue

case class  EDNLong(value: Long) extends EDNValue
case class  EDNBigInt(value: BigInt) extends EDNValue

case class  EDNDouble(value: Double) extends EDNValue
case class  EDNBigDec(value: BigDecimal) extends EDNValue

// object EDNValue {
//   // class EDNAny(val underlying: Any) extends AnyVal

//   def generic[A, B <: EDNValue](_to: A => B, _from: B => A): Generic.Aux[A, B] = new Generic[A] {
//     type Repr = B

//     def to(a: A) = _to(a)
//     def from(b: B) = _from(b)
//   }

//   implicit val genBoolean = generic(EDNBoolean.apply _, Function.unlift(EDNBoolean.unapply))
//   implicit val genLong    = generic(EDNLong.apply _,    Function.unlift(EDNLong.unapply))
//   implicit val genString  = generic(EDNString.apply _,  Function.unlift(EDNString.unapply))
//   implicit val genChar    = generic(EDNChar.apply _,    Function.unlift(EDNChar.unapply))
//   implicit val genBigInt  = generic(EDNBigInt.apply _,  Function.unlift(EDNBigInt.unapply))
//   implicit val genDouble  = generic(EDNDouble.apply _,  Function.unlift(EDNDouble.unapply))
//   implicit val genBigDec  = generic(EDNBigDec.apply _,  Function.unlift(EDNBigDec.unapply))
// }