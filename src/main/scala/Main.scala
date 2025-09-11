package nop

import frontend.lexer.lex
import frontend.parser.Parser
import frontend.support.PrettyPrintable
import frontend.parser.ParseError

import driver.printDiagnostic
import driver.Diagnostic

@main def main(args: String*): Unit =
  val filename = args(0)
  val source   = scala.io.Source.fromFile(filename)
  val input =
    try source.mkString.replace("\t", "    ")
    finally source.close()

  val lexemes = lex(input)
  Parser(lexemes).parse match
    case Left(err)           => printParseError(filename, input, err)
    case Right((fns, types)) => fns.map(_.toTree).foreach(print)

private def printParseError(filename: String, contents: String, err: ParseError): Unit =
  val diag = Diagnostic(err.toString, err.span)
  printDiagnostic(filename, contents, diag)
