package nop;
package ir;
package Ops;

import nop.typesystem.Type.Type

/** Represents instruction in the IR. */
sealed trait Instruction {
  def isTerminator: Boolean = false
}

sealed trait Operand

sealed trait Imm extends Operand

sealed trait Label

case class Param(t: Type, n: String)

case class Fn(blocks: Vector[Instruction], params: Vector[Param]) extends Label

/** Represents basic block in IR.
  *
  * In the future should have parameters instead of phi-nodes.
  * @param instrs
  */
case class BasicBlock(instrs: Vector[Instruction]) extends Label {
  private def apply(instrs: Vector[Instruction]): BasicBlock = BasicBlock(instrs)
  def addInstruction(i: Instruction): BasicBlock             = BasicBlock(instrs :+ i)
  def map(f: Instruction => Instruction): BasicBlock         = BasicBlock(instrs map f)
}

enum CmpType {
  case LE
  case GE
  case EQ
  case NEQ
  case LT
  case GT
}

class BinOp(result: Var, lhs: Operand, rhs: Operand) extends Instruction

case class Add(res: Var, lhs: Operand, rhs: Operand) extends BinOp(res, lhs, rhs)

case class Sub(res: Var, lhs: Operand, rhs: Operand) extends BinOp(res, lhs, rhs)

case class Div(res: Var, lhs: Operand, rhs: Operand) extends BinOp(res, lhs, rhs)

case class Mul(res: Var, lhs: Operand, rhs: Operand) extends BinOp(res, lhs, rhs)

case class Jmp(label: Label) extends Instruction {
  override def isTerminator: Boolean = true
}

case class Cmp(t: CmpType, result: Var, lhs: Operand, rhs: Operand) extends BinOp(result, lhs, rhs)

case class Br(cond: Operand, tlabel: Label, flabel: Label) extends Instruction {
  override def isTerminator: Boolean = true
}

case class Var(t: Type, name: String) extends Operand

case class Call(res: Var, fn: Fn, args: Vector[Param])

/** Later should be adapted for different Integer types.
  *
  * @param value
  *   Integer value.
  */
case class IRInt(value: Int) extends Imm

/** Immediate boolean
  *
  * @param value
  */
case class IRBool(value: Boolean) extends Imm
