package nop.frontend
package ast

import lexer.Token

type Precedence = Int;

enum Associativity:
  case Left
  case Right

// enum UnaryOp:
//  case Negate

enum BinaryOp {
  case Assign
  case Plus
  case Minus
  case Mul
  case Eq
  case NotEq
  case Less
  case LessEq
  case Greater
  case GreaterEq
  case And
  case Or

  /** Binary operator precedence for parsing. */
  def prec: Precedence =
    this match {
      case Assign                              => 1
      case Or                                  => 2
      case And                                 => 3
      case Eq | NotEq                          => 4
      case Less | LessEq | Greater | GreaterEq => 5
      case Plus | Minus                        => 6
      case Mul                                 => 7
    }

  /** Binary operator associativity for parsing. */
  def assoc: Associativity =
    this match {
      case Assign => Associativity.Right
      case Plus | Minus | Mul | Eq | NotEq | Less | LessEq | Greater | GreaterEq | And | Or =>
        Associativity.Left
    }

  /** Check if the operator is some form of comparison, meaning it will return `bool`, instead of
    * `T`.
    *
    * This may change when operators are handled as proper method calls.
    */
  def isCmp: Boolean =
    this match {
      case Eq | NotEq | Less | LessEq | Greater | GreaterEq => true
      case _                                                => false
    }

  /** Check if the operator is logical, i.e. only applicable to bool arguments. */
  def isLogical: Boolean =
    this match {
      case And | Or => true
      case _        => false
    }

  override def toString: String =
    this match
      case Assign => "="
      case Plus => "+"
      case Minus => "-"
      case Mul => "*"
      case Eq => "=="
      case NotEq => "!="
      case Less => "<"
      case LessEq => "<="
      case Greater => ">"
      case GreaterEq => ">="
      case And => "&&"
      case Or => "||"
}

object BinaryOp {
  def fromToken(token: Token): Option[BinaryOp] =
    token match {
      case Token.Plus      => Some(BinaryOp.Plus)
      case Token.Minus     => Some(BinaryOp.Minus)
      case Token.Mul       => Some(BinaryOp.Mul)
      case Token.Assign    => Some(BinaryOp.Assign)
      case Token.Eq        => Some(BinaryOp.Eq)
      case Token.NotEq     => Some(BinaryOp.NotEq)
      case Token.Less      => Some(BinaryOp.Less)
      case Token.LessEq    => Some(BinaryOp.LessEq)
      case Token.Greater   => Some(BinaryOp.Greater)
      case Token.GreaterEq => Some(BinaryOp.GreaterEq)
      case Token.And       => Some(BinaryOp.And)
      case Token.Or        => Some(BinaryOp.Or)
      case _               => None
    }
}
