import scaledn._
import macros._
import write._

object HelloEDN {
  def main(args: Array[String]) {
    val edn = EDN("""{"foo" 1, "bar" true, "baz" (1.2 2.3 3.4)}""")
    println("EDN:"+edn)
    assert(toEDNString(edn) == """{"foo" 1, "bar" true, "baz" (1.2 2.3 3.4)}""")
  }
}
