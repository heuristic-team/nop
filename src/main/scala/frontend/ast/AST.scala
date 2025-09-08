package nop
package frontend
package ast

import scala.collection.mutable.HashMap

import typesystem.Type

import lexer.Span
import lexer.WithSpan

import support.PrettyPrintable
import support.Tree

class AST(val decls: HashMap[String, FnDecl] = HashMap()) extends PrettyPrintable {
  override def toTree: Tree =
    Tree(
      "AST", // probably will put filename here
      decls.values.map(_.toTree).toList,
    )
}
