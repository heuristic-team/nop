package nop.frontend
package parser

import lexer.Span

type Res[T] = Either[ParseError, T];

/** Custom type for parse error.
  *
  * Should not be created manually in *most* of the cases, `get` or `get_mut` should produce these
  * errors. Exceptions are some tricky situations like trailing comma.
  */
case class ParseError(val expected: Seq[Any], val actual: Any, val span: Span):
  /** Replace `expected` with given list. */
  def expect(expected: Any*): ParseError = ParseError(expected, actual, span)
  override def toString: String = "expected " + formatExpected(expected.toList) + ", but got " + actual

private def formatExpected(expected: List[Any]): String =
  expected match
    case Nil           => ???
    case x :: Nil      => x.toString
    case x :: y :: Nil => s"$x or $y"
    case x :: xs       => s"$x, " + formatExpected(xs)
