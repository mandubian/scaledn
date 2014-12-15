package scaledn

package object parser {
  import org.parboiled2.ParserInput
  import scala.util.{Try, Success, Failure}

  def parseEDN(in: ParserInput): Try[Seq[EDN]] = EDNParser(in).Root.run() //.map(_.map(EDN(_)))
  def parseEDNFirst(in: ParserInput): Try[EDN] = EDNParser(in).Root.run().map(_.head) //.map(s => EDN(s.head))

}