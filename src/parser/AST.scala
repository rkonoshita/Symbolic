package parser

/**
 * Created by rkonoshita on 14/11/19.
 */

//意味解析用のクラス群
trait AST

case class Add(left: AST, right: AST) extends AST

case class AddSign(left: AST, right: AST) extends AST

case class AddExtends(left: AST, right: AST) extends AST

case class And(left: AST, right: AST) extends AST

case class Andc(imm: AST) extends AST

case class Band(left: AST, right: AST) extends AST

case class Bra(num: AST, size: Int) extends AST

case class Brn(num: AST, size: Int) extends AST

case class Bhi(num: AST, size: Int) extends AST

case class Bls(num: AST, size: Int) extends AST

case class Bcc(num: AST, size: Int) extends AST

case class Bcs(num: AST, size: Int) extends AST

case class Bne(num: AST, size: Int) extends AST

case class Beq(num: AST, size: Int) extends AST

case class Bvc(num: AST, size: Int) extends AST

case class Bvs(num: AST, size: Int) extends AST

case class Bpl(num: AST, size: Int) extends AST

case class Bmi(num: AST, size: Int) extends AST

case class Bge(num: AST, size: Int) extends AST

case class Blt(num: AST, size: Int) extends AST

case class Bgt(num: AST, size: Int) extends AST

case class Ble(num: AST, size: Int) extends AST

case class Bclr(left: AST, right: AST) extends AST

case class Biand(left: AST, right: AST) extends AST

case class Bild(left: AST, right: AST) extends AST

case class Bior(left: AST, right: AST) extends AST

case class Bist(left: AST, right: AST) extends AST

case class Bixor(left: AST, right: AST) extends AST

case class Bld(left: AST, right: AST) extends AST

case class Bnot(left: AST, right: AST) extends AST

case class Bor(left: AST, right: AST) extends AST

case class Bset(left: AST, right: AST) extends AST

case class Bsr(disp: AST, size: Int) extends AST

case class Bst(left: AST, right: AST) extends AST

case class Btst(left: AST, right: AST) extends AST

case class Bxor(left: AST, right: AST) extends AST

case class Cmp(left: AST, right: AST) extends AST

case class Daa(reg: AST) extends AST

case class Das(reg: AST) extends AST

case class Dec(left: AST, right: AST) extends AST

case class Divxs(left: AST, right: AST) extends AST

case class Divxu(left: AST, right: AST) extends AST

case class Eepmov(size: Int) extends AST

case class Exts(reg: AST) extends AST

case class Extu(reg: AST) extends AST

case class Inc(left: AST, right: AST) extends AST

case class Jmp(add: AST) extends AST

case class Jsr(add: AST) extends AST

case class Ldc(reg: AST) extends AST

case class Mov(left: AST, right: AST) extends AST

case class Movfpe(left: AST, right: AST) extends AST

case class Movtpe(left: AST, right: AST) extends AST

case class Mulxs(left: AST, right: AST) extends AST

case class Mulxu(left: AST, right: AST) extends AST

case class Neg(reg: AST) extends AST

case class Nop() extends AST

case class Not(reg: AST) extends AST

case class Or(left: AST, right: AST) extends AST

case class Orc(imm: AST) extends AST

case class Pop(reg: AST) extends AST

case class Push(reg: AST) extends AST

case class Rotl(reg: AST) extends AST

case class Rotr(reg: AST) extends AST

case class Rotxl(reg: AST) extends AST

case class Rotxr(reg: AST) extends AST

case class Rte() extends AST

case class Rts() extends AST

case class Shal(reg: AST) extends AST

case class Shar(reg: AST) extends AST

case class Shll(reg: AST) extends AST

case class Shlr(reg: AST) extends AST

case class Sleep() extends AST

case class Stc(reg: AST) extends AST

case class Sub(left: AST, right: AST) extends AST

case class Subs(left: AST, right: AST) extends AST

case class Subx(left: AST, right: AST) extends AST

case class Trapa(imm: AST) extends AST

case class Xor(left: AST, right: AST) extends AST

case class Xorc(imm: AST) extends AST

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

case class LabelName(name: String) extends AST

case class MakeLabel(label: AST) extends AST

case class Data(num: AST, size: Int) extends AST

case class DataBlock(block: AST, data: AST, size: Int) extends AST

case class Expr(op: String, left: AST, right: AST) extends AST

case class Minus(num: AST) extends AST

case class Rev(num: AST) extends AST

case class High(num: AST) extends AST

case class Low(num: AST) extends AST

case class HWord(num: AST) extends AST

case class LWord(num: AST) extends AST

case class StartOf(sec: String) extends AST

case class SizeOf(sec: String) extends AST

case class Empty() extends AST