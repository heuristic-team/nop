package nop
package typesystem

import frontend.lexer.WithSpan

case class TypeDecl(name: WithSpan[String], value: WithSpan[Type])
