package nop;
package frontend;
package ast;

import typesystem.Type
import lexer.Span
import lexer.WithSpan

sealed trait Expr {
  def ty: Type
  def span: Span
}

case class Declare(isMut: Boolean, name: WithSpan[String], varTy: WithSpan[Type], value: Expr)
    extends Expr {
  override def ty: Type   = Type.Unit
  override def span: Span = Span(name.span.start, value.span.end)
}

case class Ret(value: Option[Expr], span: Span) extends Expr {
  override def ty: Type = Type.Bottom
}

case class Block(ty: Type, body: List[Expr], span: Span) extends Expr

case class While(
    // ty: Type, // TODO, see issue #18
    cond: Expr,
    body: Expr,
    span: Span,
) extends Expr {
  override def ty: Type = Type.Unit
}

case class If(
    ty: Type,
    cond: Expr,
    onTrue: Expr,
    onFalse: Option[Expr],
    kwSpan: Span,
    inStmtPos: Boolean,
) extends Expr {
  override def span: Span = Span(kwSpan.start, onTrue.span.end)
}

case class Num(ty: Type, value: WithSpan[Int]) extends Expr {
  override def span: Span = value.span
}

case class Bool(value: Boolean, span: Span) extends Expr {
  override def ty: Type = Type.Bool
}

case class Ref(ty: Type, name: WithSpan[String]) extends Expr {
  override def span: Span = name.span
}

case class MemberRef(ty: Type, target: Expr, member: WithSpan[String]) extends Expr {
  override def span: Span = member.span
}

case class Call(ty: Type, callee: Expr, args: List[Expr], span: Span) extends Expr

// Unary {
//     op: UnaryOp,
//     operand: Box<Expr>,
// },

case class Binary(ty: Type, op: WithSpan[BinaryOp], lhs: Expr, rhs: Expr) extends Expr {
  override def span: Span = op.span
}
