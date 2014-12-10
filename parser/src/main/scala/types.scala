import shapeless.Generic

sealed trait EDN

case class  EDNSymbol(value: String, namespace: Option[String] = None) extends EDN
case class  EDNKeyword(value: EDNSymbol) extends EDN
case class  EDNTagged[A](tag: EDNSymbol, value: A) extends EDN

case object EDNNil extends EDN
case class  EDNBoolean(value: Boolean) extends EDN
case class  EDNString(value: String) extends EDN
case class  EDNChar(value: Char) extends EDN

case class  EDNLong(value: Long) extends EDN
case class  EDNBigInt(value: BigInt) extends EDN

case class  EDNDouble(value: Double) extends EDN
case class  EDNBigDec(value: BigDecimal) extends EDN


object EDN {

  def generic[A, B <: EDN](_to: A => B, _from: B => A): Generic.Aux[A, B] = new Generic[A] {
    type Repr = B

    def to(a: A) = _to(a)
    def from(b: B) = _from(b)
  }

  implicit val genBoolean = generic(EDNBoolean.apply _, Function.unlift(EDNBoolean.unapply))
  implicit val genLong    = generic(EDNLong.apply _,    Function.unlift(EDNLong.unapply))
  implicit val genString  = generic(EDNString.apply _,  Function.unlift(EDNString.unapply))
  implicit val genChar    = generic(EDNChar.apply _,    Function.unlift(EDNChar.unapply))
  implicit val genBigInt  = generic(EDNBigInt.apply _,  Function.unlift(EDNBigInt.unapply))
  implicit val genDouble  = generic(EDNDouble.apply _,  Function.unlift(EDNDouble.unapply))
  implicit val genBigDec  = generic(EDNBigDec.apply _,  Function.unlift(EDNBigDec.unapply))
}