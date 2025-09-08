package nop

import frontend.lexer.lex
import frontend.parser.Parser
import frontend.support.PrettyPrintable
import frontend.parser.ParseError

@main def main(): Unit =
  val source = scala.io.Source.fromFile("examples/basic.nop")
  val input =
    try source.mkString.replace("\t", "    ")
    finally source.close()

  val lexemes = lex(input)
  Parser(lexemes).parse match
    case Left(err)           => printError(input, err)
    case Right((fns, types)) => fns.map(_.toTree).foreach(print)

private def printError(input: String, err: ParseError): Unit =
  ???
