package object scaledn {
  import org.parboiled2.ParserInput
  import scala.util.{Try, Success, Failure}

  type EDN = Any

  object EDN {
    def parse(in: ParserInput): Try[Seq[EDN]] = EDNParser(in).Root.run()
    def parseFirst(in: ParserInput): Try[EDN] = EDNParser(in).Root.run().map(_.head)
  }
}