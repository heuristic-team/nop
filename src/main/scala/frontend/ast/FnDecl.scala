package nop;
package frontend;
package ast;

import lexer.WithSpan

import support.PrettyPrintable
import support.Tree

import typesystem.Type

case class FnParam(isMut: Boolean, name: WithSpan[String], ty: WithSpan[Type]) {
  override def toString: String =
    (if isMut then "mut " else "") + s"${name.value}: ${ty.value}"
}

case class FnDecl(
    name: WithSpan[String],
    rettype: WithSpan[Type],
    params: List[FnParam],
    body: Expr,
) extends PrettyPrintable {
  override def toTree: Tree =
    val self = s"fn ${name.value} " + params.mkString("(", ", ", ") ") + rettype.value
    Tree(self, body.toTree :: Nil)
}
