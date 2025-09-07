package nop;
package frontend;
package lexer;

class Span(val start: Int, val end: Int)

class WithSpan[T](val value: T, val span: Span):
  def replace[U](value: U): WithSpan[U] = WithSpan(value, span)
  def map[U](f: T => U): WithSpan[U] = WithSpan(f(value), span)
