package nop;
package frontend;
package ast;

import scala.collection.mutable.HashMap

import typesystem.Type
import lexer.Span
import lexer.WithSpan

class AST(val decls: HashMap[String, FnDecl] = HashMap())

case class FnParam(isMut: Boolean, name: WithSpan[String])
case class FnDecl(
    name: WithSpan[String],
    rettype: WithSpan[Type],
    params: List[FnParam],
    body: Expr,
)


