package nop
package frontend
package ast

import typesystem.Type
import lexer.Span
import lexer.WithSpan

import support.PrettyPrintable
import nop.frontend.support.Tree

sealed trait Expr extends PrettyPrintable:
  def ty: Type
  def span: Span

case class Declaration(
    isMut: Boolean,
    name: WithSpan[String],
    varTy: WithSpan[Type],
    value: Expr,
) extends Expr {
  override def ty: Type   = Type.Unit
  override def span: Span = Span(name.span.start, value.span.end)

  override def toTree: Tree =
    val mut  = if isMut then "mut " else ""
    val self = s"Declare $mut${name.value}: ${varTy.value}"
    Tree(self, value.toTree :: Nil)
}

case class Ret(value: Option[Expr], span: Span) extends Expr:
  override def ty: Type     = Type.Bottom
  override def toTree: Tree = Tree("Ret", value.map(_.toTree).toList)

case class Block(ty: Type, body: List[Expr], span: Span) extends Expr:
  override def toTree: Tree = Tree(s"Block $ty", body.map(_.toTree))

case class While(
    // ty: Type, // TODO, see issue #18
    cond: Expr,
    body: Expr,
    span: Span,
) extends Expr:
  override def ty: Type     = Type.Unit
  override def toTree: Tree = Tree("While", cond.toTree :: body.toTree :: Nil)

case class If(
    ty: Type,
    cond: Expr,
    onTrue: Expr,
    onFalse: Option[Expr],
    kwSpan: Span,
    inStmtPos: Boolean,
) extends Expr:
  override def span: Span = Span(kwSpan.start, onTrue.span.end)
  override def toTree: Tree =
    val pos = if inStmtPos then "stmt" else "expr"
    Tree(s"If $ty $pos", onTrue.toTree :: onFalse.map(_.toTree).toList)

case class NumLit(ty: Type, value: WithSpan[Int]) extends Expr:
  override def span: Span   = value.span
  override def toTree: Tree = Tree(s"NumLit $ty ${value.value}", Nil)

case class BoolLit(value: Boolean, span: Span) extends Expr:
  override def ty: Type     = Type.Bool
  override def toTree: Tree = Tree(s"BoolLit ${value}", Nil)

case class Ref(ty: Type, name: WithSpan[String]) extends Expr:
  override def span: Span   = name.span
  override def toTree: Tree = Tree(s"Ref $ty ${name.value}", Nil)

case class MemberRef(ty: Type, target: Expr, member: WithSpan[String]) extends Expr:
  override def span: Span   = member.span
  override def toTree: Tree = Tree(s"MemberRef $ty ${member.value}", target.toTree :: Nil)

case class Call(ty: Type, callee: Expr, args: List[Expr], span: Span) extends Expr:
  override def toTree: Tree =
    val argsTree = Tree("Args", args.map(_.toTree))
    Tree(s"Call $ty", callee.toTree :: argsTree :: Nil)

// Unary {
//     op: UnaryOp,
//     operand: Box<Expr>,
// },

case class Binary(ty: Type, op: WithSpan[BinaryOp], lhs: Expr, rhs: Expr) extends Expr:
  override def span: Span   = op.span
  override def toTree: Tree = Tree(s"Binary $ty ${op.value}", lhs.toTree :: rhs.toTree :: Nil)
