package nop
package typesystem

import frontend.lexer.WithSpan

case class Field(val name: String, val ty: WithSpan[Type])

object Type {
  def primitiveFromString(s: String): Option[Type] =
    s match
      case "i64"  => Some(I64)
      case "bool" => Some(Bool)
      case "unit" => Some(Unit)
      case _      => None
}

enum Type {
  case Undef
  case Bottom
  case Unit
  case I64
  case Bool
  case Function(val params: List[Type], val rettype: Type)
  case Struct(val name: WithSpan[String], val fields: List[Field])
  case Alias(val target: String)

  def isPrimitive: Boolean =
    this match
      case Bottom | Unit | I64 | Bool | Undef => true
      case _                                  => false

  def isInteger: Boolean =
    this match
      case I64 => true
      case _   => false

  override def toString: String =
    this match
      case Bottom      => "_"
      case Unit        => "unit"
      case Bool        => "bool"
      case I64         => "i64"
      case Undef       => "?"
      case Alias(name) => name
      case Function(params, rettype) =>
        params.map(_.toString).mkString("fn (", ", ", s") -> $rettype")
      case Struct(_, _) => ??? // this `???` is valid because we have no anonymous structs
}
