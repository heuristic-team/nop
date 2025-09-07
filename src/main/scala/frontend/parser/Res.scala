package nop.frontend
package parser

import lexer.Span

type Res[T] = Either[ParseError, T];

/** Custom type for parse error.
  *
  * Should not be created manually in *most* of the cases, `get` or `get_mut` should produce these
  * errors. Exceptions are some tricky situations like trailing comma.
  */
case class ParseError(val expected: Seq[Any], val actual: Any, val span: Span) {

  /** Replace `expected` with given list. */
  def expect(expected: Any*): ParseError = ParseError(expected, actual, span)

  override def toString: String =
    assert(!expected.isEmpty)
    "expected " + expected.init.init.mkString(", ") + " or " + expected.last + ", but got " + actual
}
