package parser

import java.io.File

import data.register.Memory
import z3.scala.{Z3Context, Z3AST}

import scala.collection.mutable
import scala.io.Source

/**
 * Created by rkonoshita on 14/11/20.
 */
class ASTVisitor {

  val label = new mutable.HashMap[String, Int]
  label += "V" -> 0
  label += "P" -> 0x0100
  label += "C" -> 0x0100
  label += "D" -> 0x0100
  label += "B" -> 0xE800
  label += "R" -> 0xE800
  val count = new MyHashMap
  var section = ""

  def makeProgram(ctx: Z3Context, file: File): Memory = {
    file.listFiles.foreach { f =>
      println(f)
      Source.fromFile(f).getLines.foreach(l => println(new ASTParser().parse(l)))
    }
    new Memory(ctx, new mutable.HashMap[Int, Z3AST]())
  }

  def countup(up: Int) =
    if (!count.contains(section)) count += section -> up
    else count += section -> (count(section) + up)

  def search(ast: AST): VisitItem = {
    ast match {
      case Add(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitInt(2)
          case (l: RegByte, r: RegByte) => new VisitInt(2)
          case (l: Imm, r: RegWord) => new VisitInt(4)
          case (l: RegWord, r: RegWord) => new VisitInt(2)
          case (l: Imm, r: RegLong) => new VisitInt(6)
          case (l: RegLong, r: RegLong) => new VisitInt(2)
        }
      case Inc(left, right) => new VisitInt(2)
      case Cmp(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitInt(2)
          case (l: RegByte, r: RegByte) => new VisitInt(2)
          case (l: Imm, r: RegWord) => new VisitInt(4)
          case (l: RegWord, r: RegWord) => new VisitInt(2)
          case (l: Imm, r: RegLong) => new VisitInt(6)
          case (l: RegLong, r: RegLong) => new VisitInt(2)
        }
      case Sub(left, right) =>
        (left, right) match {
          case (l: RegByte, r: RegByte) => new VisitInt(2)
          case (l: Imm, r: RegWord) => new VisitInt(4)
          case (l: RegWord, r: RegWord) => new VisitInt(2)
          case (l: Imm, r: RegLong) => new VisitInt(6)
          case (l: RegLong, r: RegLong) => new VisitInt(2)
        }
      case And(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitInt(2)
          case (l: RegByte, r: RegByte) => new VisitInt(2)
          case (l: Imm, r: RegWord) => new VisitInt(4)
          case (l: RegWord, r: RegWord) => new VisitInt(2)
          case (l: Imm, r: RegLong) => new VisitInt(6)
          case (l: RegLong, r: RegLong) => new VisitInt(4)
        }
      case Not(item) => new VisitInt(2)
      case Andc(item) => new VisitInt(2)
      case Orc(item) => new VisitInt(2)
      case Mov(left, right) =>
        (left, right) match {
          case (l: RegByte, r: RegByte) => new VisitInt(2)
          case (l: RegWord, r: RegWord) => new VisitInt(2)
          case (l: RegLong, r: RegLong) => new VisitInt(2)
          case (l: Imm, r: RegByte) => new VisitInt(2)
          case (l: IndirReg, r: RegByte) => new VisitInt(2)
          case (l: Disp, r: RegByte) => new VisitInt(if (visit(l).asInstanceOf[VisitInt].item == 16) 4 else 8)
          case (l: Pos, r: RegByte) => new VisitInt(2)
          case (l: AbsAddress, r: RegByte) =>
            val vi = visit(l).asInstanceOf[VisitInt].item
            new VisitInt(if (vi == 8) 2 else if (vi == 16) 4 else 6)
          case (l: Imm, r: RegWord) => new VisitInt(4)
          case (l: IndirReg, r: RegWord) => new VisitInt(2)
          case (l: Disp, r: RegWord) => new VisitInt(if (visit(l).asInstanceOf[VisitInt].item == 16) 4 else 8)
          case (l: Pos, r: RegWord) => new VisitInt(2)
          case (l: AbsAddress, r: RegWord) => new VisitInt(if (visit(l).asInstanceOf[VisitInt].item == 16) 4 else 6)
          case (l: Imm, r: RegLong) => new VisitInt(6)
          case (l: IndirReg, r: RegLong) => new VisitInt(4)
          case (l: Disp, r: RegLong) => new VisitInt(if (visit(l).asInstanceOf[VisitInt].item == 16) 6 else 10)
          case (l: Pos, r: RegLong) => new VisitInt(4)
          case (l: AbsAddress, r: RegLong) => new VisitInt(if (visit(l).asInstanceOf[VisitInt].item == 16) 6 else 8)
          case (l: RegByte, r: IndirReg) => new VisitInt(2)
          case (l: RegByte, r: Disp) => new VisitInt(if (visit(r).asInstanceOf[VisitInt].item == 16) 4 else 8)
          case (l: RegByte, r: Pre) => new VisitInt(2)
          case (l: RegByte, r: AbsAddress) =>
            val vi = visit(r).asInstanceOf[VisitInt].item
            new VisitInt(if (vi == 8) 2 else if (vi == 16) 4 else 6)
          case (l: RegWord, r: IndirReg) => new VisitInt(2)
          case (l: RegWord, r: Disp) => new VisitInt(if (visit(r).asInstanceOf[VisitInt].item == 16) 4 else 8)
          case (l: RegWord, r: Pre) => new VisitInt(2)
          case (l: RegWord, r: AbsAddress) => new VisitInt(if (visit(r).asInstanceOf[VisitInt].item == 16) 4 else 6)
          case (l: RegLong, r: IndirReg) => new VisitInt(4)
          case (l: RegLong, r: Disp) => new VisitInt(if (visit(r).asInstanceOf[VisitInt].item == 16) 6 else 10)
          case (l: RegLong, r: Pre) => new VisitInt(4)
          case (l: RegLong, r: AbsAddress) => new VisitInt(if (visit(r).asInstanceOf[VisitInt].item == 16) 4 else 6)
        }
      case Pop(reg) =>
        reg match {
          case (r: RegWord) => new VisitInt(2)
          case (r: RegLong) => new VisitInt(4)
        }
      case Push(reg) =>
        reg match {
          case (r: RegWord) => new VisitInt(2)
          case (r: RegLong) => new VisitInt(4)
        }
      case Extu(reg) =>
        reg match {
          case (r: RegWord) => new VisitInt(2)
          case (r: RegLong) => new VisitInt(4)
        }
      case Bclr(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitInt(2)
          case (l: Imm, r: IndirReg) => new VisitInt(4)
          case (l: Imm, r: AbsAddress) => new VisitInt(4)
          case (l: RegByte, r: RegByte) => new VisitInt(2)
          case (l: RegByte, r: IndirReg) => new VisitInt(4)
          case (l: RegByte, r: AbsAddress) => new VisitInt(4)
        }
      case Bset(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitInt(2)
          case (l: Imm, r: IndirReg) => new VisitInt(4)
          case (l: Imm, r: AbsAddress) => new VisitInt(4)
          case (l: RegByte, r: RegByte) => new VisitInt(2)
          case (l: RegByte, r: IndirReg) => new VisitInt(4)
          case (l: RegByte, r: AbsAddress) => new VisitInt(4)
        }
      case Jmp(add) =>
        add match {
          case (a: IndirReg) => new VisitInt(2)
          case (a: AbsAddress) => new VisitInt(4)
          case (a: IndirAdd) => new VisitInt(2)
        }
      case Jsr(add) =>
        add match {
          case (a: IndirReg) => new VisitInt(2)
          case (a: AbsAddress) => new VisitInt(4)
          case (a: IndirAdd) => new VisitInt(2)
        }
      case Bra(num, size) => new VisitInt(if (size == 8) 2 else 4)
      case Blo(num, size) => new VisitInt(if (size == 8) 2 else 4)
      case Blt(num, size) => new VisitInt(if (size == 8) 2 else 4)
      case Bhi(num, size) => new VisitInt(if (size == 8) 2 else 4)
      case Rte() => new VisitInt(2)
      case Rte() => new VisitInt(2)
      case Data(num, size) =>
        size match {
          case 8 => new VisitInt(1)
          case 16 => new VisitInt(2)
          case 32 => new VisitInt(4)
        }
      case DataBlock(block, data, size) =>
        size match {
          case 8 => new VisitInt(1 * size)
          case 16 => new VisitInt(2 * size)
          case 32 => new VisitInt(4 * size)
        }
      case AbsAddress(num, size) => new VisitInt(size)
      case Disp(disp, reg, size) => new VisitInt(size)
      case Label(str) => new VisitString(str)
      case MakeLabel(name) =>
        label += name.asInstanceOf[VisitString].item -> count(section)
        new VisitInt(0)
      case Section(sec) =>
        section = sec
        new VisitInt(0)
    }
  }

  //とりあえず置いといて
  def visit(ast: AST): VisitItem = {
    ast match {
      case Add(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitArray(Array(0x80 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item))
          case (l: RegByte, r: RegByte) => new VisitArray(Array(0x08, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item))
          case (l: Imm, r: RegWord) =>
            val hold = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(Array(0x79, 0x10 | visit(r).asInstanceOf[VisitInt].item, (hold >> 8) & 0xFF, hold & 0xFF))
          case (l: RegWord, r: RegWord) => new VisitArray(Array(0x09, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item))
          case (l: Imm, r: RegLong) =>
            val hold = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(Array(0x7A, 0x10 | visit(r).asInstanceOf[VisitInt].item, (hold >> 24) & 0xFF, (hold >> 16) & 0xFF, (hold >> 8) & 0xFF, hold & 0xFF))
          case (l: RegLong, r: RegLong) => new VisitArray(Array(0x0A, 0x10 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item))
        }
      case Inc(left, right) =>
        (left, right) match {
          case (l: RegByte, r: Number) => new VisitArray(Array(0x0A, visit(l).asInstanceOf[VisitInt].item))
          case (l: Imm, r: RegWord) => new VisitArray(Array(0x0B, if (visit(l).asInstanceOf[VisitInt].item == 1) 0x50 | visit(r).asInstanceOf[VisitInt].item else visit(r).asInstanceOf[VisitInt].item))
          case (l, Imm, r: RegLong) => new VisitArray(Array(0x80, if (visit(l).asInstanceOf[VisitInt].item == 1) 0x70 | visit(r).asInstanceOf[VisitInt].item else 0xF0 | visit(r).asInstanceOf[VisitInt].item))
        }
      case Cmp(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) =>
          case (l: RegByte, r: RegByte) =>
          case (l: Imm, r: RegWord) =>
          case (l: RegWord, r: RegWord) =>
          case (l: Imm, RegLong, r: RegLong) =>
          case (l, RegLong, r: RegLong) =>
        }
      case Section(str) => new VisitString(str)
      case RegByte(num) => new VisitInt(num)
      case RegWord(num) => new VisitInt(num)
      case RegLong(num) => new VisitInt(num)
      case Number(num) => new VisitInt(num)
    }
  }

}

trait VisitItem {
  type T
  val item: T
}

class VisitInt(num: Int) extends VisitItem {
  type T = Int
  val item = num
}

class VisitArray(num: Array[Int]) extends VisitItem {
  type T = Array[Int]
  val item = num
}

class VisitString(num: String) extends VisitItem {
  type T = String
  val item = num
}

class MyHashMap extends mutable.HashMap[String, Int] {

  override def put(key: String, value: Int): Option[Int] = {
    key match {
      case "V" =>
        super.put("V", value)
      case "P" =>
        super.put("P", value)
        super.put("C", value)
        super.put("D", value)
      case "C" =>
        super.put("C", value)
        super.put("D", value)
      case "D" =>
        super.put("D", value)
      case "B" =>
        super.put("B", value)
        super.put("R", value)
      case "R" =>
        super.put("R", value)
    }
  }
}

