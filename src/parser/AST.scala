package parser

/**
 * Created by rkonoshita on 14/11/19.
 */
trait AST

case class Add(left: AST, right: AST) extends AST

case class Inc(left: AST, right: AST) extends AST

case class Cmp(left: AST, right: AST) extends AST

case class Sub(left: AST, right: AST) extends AST

case class And(left: AST, right: AST) extends AST

case class Not(reg: AST) extends AST

case class Andc(imm: AST) extends AST

case class Orc(imm: AST) extends AST

case class Mov(left: AST, right: AST) extends AST

case class Push(reg: AST) extends AST

case class Pop(reg: AST) extends AST

case class Extu(reg: AST) extends AST

case class Bset(left: AST, right: AST) extends AST

case class Bclr(left: AST, right: AST) extends AST

case class Jsr(add: AST) extends AST

case class Jmp(add: AST) extends AST

case class Rts() extends AST

case class Rte() extends AST

case class Bra(num: AST, size: Int) extends AST

case class Blo(num: AST, size: Int) extends AST

case class Blt(num: AST, size: Int) extends AST

case class Bhi(num: AST, size: Int) extends AST

case class Section(str: String) extends AST

case class RegByte(num: Int) extends AST

case class RegWord(num: Int) extends AST

case class RegLong(num: Int) extends AST

case class AbsAddress(num: AST, size: Int) extends AST

case class IndirReg(reg: AST) extends AST

case class IndirAdd(num: AST) extends AST

case class Imm(num: AST) extends AST

case class Number(num: Int) extends AST

case class Disp(disp: AST, reg: AST, size: Int) extends AST

case class Pos(reg: AST) extends AST

case class Pre(reg: AST) extends AST

case class Label(name: String) extends AST

case class MakeLabel(label: AST) extends AST

case class Data(num: AST) extends AST

case class DataBlock(num1: AST, num2: AST) extends AST

case class Expr(op: String, left: AST, right: AST) extends AST

case class Minus(num: AST) extends AST

case class High(num: AST) extends AST

case class Low(num: AST) extends AST

case class HWord(num: AST) extends AST

case class LWord(num: AST) extends AST

case class StartOf(sec: String) extends AST

case class SizeOf(sec: String) extends AST