package parser

/**
 * Created by rkonoshita on 14/11/19.
 */
trait AST

case class AddByte(left: AST, right: AST) extends AST

case class AddWord(left: AST, right: AST) extends AST

case class AddLong(left: AST, right: AST) extends AST

case class SubByte(left: AST, right: AST) extends AST

case class SubWord(left: AST, right: AST) extends AST

case class SubLong(left: AST, right: AST) extends AST

case class MovByte(left: AST, right: AST) extends AST

case class MovWord(left: AST, right: AST) extends AST

case class MovLong(left: AST, right: AST) extends AST

case class JumpSub(add: AST) extends AST

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

case class MakeLabel(name: String) extends AST

case class UseLabel(name: String) extends AST