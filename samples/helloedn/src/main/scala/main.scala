// import scaledn._
// import macros._
// import write._

// object HelloEDN {
//   def main(args: Array[String]) {
//     val edn = EDN("""{"foo" 1, "bar" true, "baz" (1.2 2.3 3.4)}""")
//     println("EDN:"+edn)
//     assert(toEDNString(edn) == """{"foo" 1, "bar" true, "baz" (1.2 2.3 3.4)}""")
//   }
// }


package yo

import scaledn._
import parser._

import macros._
import validate._
import shapeless._

// import play.api.data.mapping._


object HelloEDN  extends App {
  case class Address(lat:Double, lon:Double)
  case class Person (name:String, addr:Address)

  val data = """#yo.helloedn/Person {:name "yo",
     :addr #yo.helloedn/Address {:lat 0.0, :lon 0.0}}"""

  println(parseEDN(data).map(validate[Person]))

}