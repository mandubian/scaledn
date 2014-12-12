package object scaledn {
  import org.parboiled2.ParserInput
  import scala.util.{Try, Success, Failure}

  type EDN = Any

  // case class EDN(underlying: Any) extends AnyVal

  object EDN {
    def parse(in: ParserInput): Try[Seq[EDN]] = EDNParser(in).Root.run() //.map(_.map(EDN(_)))
    def parseFirst(in: ParserInput): Try[EDN] = EDNParser(in).Root.run().map(_.head) //.map(s => EDN(s.head))
  }
}