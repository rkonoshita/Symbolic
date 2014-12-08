package parser

import java.io.File

import data.register.{ROM, Memory}
import main.Parameter
import symbol.{IntSymbol, MySymbol}
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by rkonoshita on 14/11/20.
 */

//意味解析用
class ASTVisitor {

  val label = new mutable.HashMap[String, Int]
  //ラベルの位置を保持
  val count = new MyHashMap // 各セクションの終点アドレスを保持
  count ++= Parameter.getStart
  var section = ""
  val parseResult = new ListBuffer[AST]
  val tmppc = new MyHashMap
  tmppc ++= Parameter.getStart

  //ここでメモリにオペランドを配置する
  def makeProgram(ctx: Z3Context, file: File): ROM = {
    //構文解析
    file.listFiles.foreach { f => Source.fromFile(f).getLines.foreach { l => parseResult += new ASTParser().parse(l).get}}

    //意味解析１回目：ラベルの位置を決める
    parseResult.foreach { p =>
      //count.putの中にnumの中身を直接入れたらダメ
      //numを介して代入すること
      val num = search(p).asInstanceOf[VisitInt].item
      count.put(section, num)
    }

    val rom = new mutable.HashMap[Int, Byte]
    //意味解析してメモリ上にデータを配置していく
    parseResult.foreach { p =>
      println(p)
      val array = visit(p)
      if (array.isInstanceOf[VisitArray]) {
        (0 until array.asInstanceOf[VisitArray].item.length).foreach { op =>
          print("%x".format(array.asInstanceOf[VisitArray].item(op) & 0xFF) + ":")
          rom += (tmppc(section) + op) -> array.asInstanceOf[VisitArray].item(op).toByte
        }
        tmppc.put(section, array.asInstanceOf[VisitArray].item.length)
      }
      println

    }
    Parameter.sizeset(count)
    new ROM(rom)
  }

  //ラベル位置の捜索
  def search(ast: AST): Visit = {
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
          case (l: Disp, r: RegByte) => new VisitInt(if (search(l).asInstanceOf[VisitInt].item == 16) 4 else 8)
          case (l: Pos, r: RegByte) => new VisitInt(2)
          case (l: AbsAddress, r: RegByte) =>
            val vi = search(l).asInstanceOf[VisitInt].item
            new VisitInt(if (vi == 8) 2 else if (vi == 16) 4 else 6)
          case (l: Imm, r: RegWord) => new VisitInt(4)
          case (l: IndirReg, r: RegWord) => new VisitInt(2)
          case (l: Disp, r: RegWord) => new VisitInt(if (search(l).asInstanceOf[VisitInt].item == 16) 4 else 8)
          case (l: Pos, r: RegWord) => new VisitInt(2)
          case (l: AbsAddress, r: RegWord) => new VisitInt(if (search(l).asInstanceOf[VisitInt].item == 16) 4 else 6)
          case (l: Imm, r: RegLong) => new VisitInt(6)
          case (l: IndirReg, r: RegLong) => new VisitInt(4)
          case (l: Disp, r: RegLong) => new VisitInt(if (search(l).asInstanceOf[VisitInt].item == 16) 6 else 10)
          case (l: Pos, r: RegLong) => new VisitInt(4)
          case (l: AbsAddress, r: RegLong) => new VisitInt(if (search(l).asInstanceOf[VisitInt].item == 16) 6 else 8)
          case (l: RegByte, r: IndirReg) => new VisitInt(2)
          case (l: RegByte, r: Disp) => new VisitInt(if (search(r).asInstanceOf[VisitInt].item == 16) 4 else 8)
          case (l: RegByte, r: Pre) => new VisitInt(2)
          case (l: RegByte, r: AbsAddress) =>
            val vi = search(r).asInstanceOf[VisitInt].item
            new VisitInt(if (vi == 8) 2 else if (vi == 16) 4 else 6)
          case (l: RegWord, r: IndirReg) => new VisitInt(2)
          case (l: RegWord, r: Disp) => new VisitInt(if (search(r).asInstanceOf[VisitInt].item == 16) 4 else 8)
          case (l: RegWord, r: Pre) => new VisitInt(2)
          case (l: RegWord, r: AbsAddress) => new VisitInt(if (search(r).asInstanceOf[VisitInt].item == 16) 4 else 6)
          case (l: RegLong, r: IndirReg) => new VisitInt(4)
          case (l: RegLong, r: Disp) => new VisitInt(if (search(r).asInstanceOf[VisitInt].item == 16) 6 else 10)
          case (l: RegLong, r: Pre) => new VisitInt(4)
          case (l: RegLong, r: AbsAddress) => new VisitInt(if (search(r).asInstanceOf[VisitInt].item == 16) 4 else 6)
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
      case Rts() => new VisitInt(2)
      case Rte() => new VisitInt(2)
      case Data(num, size) =>
        size match {
          case 8 => new VisitInt(1)
          case 16 => new VisitInt(2)
          case 32 => new VisitInt(4)
        }
      case DataBlock(block, data, size) =>
        size match {
          case 8 => new VisitInt(1 * search(block).asInstanceOf[VisitInt].item)
          case 16 => new VisitInt(2 * search(block).asInstanceOf[VisitInt].item)
          case 32 => new VisitInt(4 * search(block).asInstanceOf[VisitInt].item)
        }
      case AbsAddress(num, size) => new VisitInt(size)
      case Disp(disp, reg, size) => new VisitInt(size)
      case LabelName(str) => new VisitString(str)
      case Number(num) => new VisitInt(num)
      case MakeLabel(name) =>
        label += search(name).asInstanceOf[VisitString].item -> count(section)
        new VisitInt(0)
      case Section(sec) =>
        section = sec
        new VisitInt(0)
    }
  }

  //メモリに命令やデータを配置
  def visit(ast: AST): Visit = {
    ast match {
      case Add(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitArray(0x80 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
          case (l: RegByte, r: RegByte) => new VisitArray(0x08, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegWord) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x79, 0x10 | visit(r).asInstanceOf[VisitInt].item, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: RegWord, r: RegWord) => new VisitArray(0x09, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegLong) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x7A, 0x10 | visit(r).asInstanceOf[VisitInt].item, (imm >> 24) & 0xFF, (imm >> 16) & 0xFF, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: RegLong, r: RegLong) => new VisitArray(0x0A, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        }
      case Inc(left, right) =>
        (left, right) match {
          case (l: RegByte, r: Number) => new VisitArray(0x0A, visit(l).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegWord) => new VisitArray(0x0B, if (visit(l).asInstanceOf[VisitInt].item == 1) 0x50 | visit(r).asInstanceOf[VisitInt].item else visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegLong) => new VisitArray(0x80, if (visit(l).asInstanceOf[VisitInt].item == 1) 0x70 | visit(r).asInstanceOf[VisitInt].item else 0xF0 | visit(r).asInstanceOf[VisitInt].item)
        }
      case Cmp(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitArray(0xA0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
          case (l: RegByte, r: RegByte) => new VisitArray(0x1C, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegWord) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x79, 0x20 | visit(r).asInstanceOf[VisitInt].item, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: RegWord, r: RegWord) => new VisitArray(0x1D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegLong) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x7A, 0x20 | visit(r).asInstanceOf[VisitInt].item, (imm >> 24) & 0xFF, (imm >> 16) & 0xFF, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: RegLong, r: RegLong) => new VisitArray(0x1F, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        }
      case Sub(left, right) =>
        (left, right) match {
          case (l: RegByte, r: RegByte) => new VisitArray(0x18, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegWord) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x79, 0x30 | visit(r).asInstanceOf[VisitInt].item, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: RegWord, r: RegWord) => new VisitArray(0x19, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegLong) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x7A, 0x30 | visit(r).asInstanceOf[VisitInt].item, (imm >> 24) & 0xFF, (imm >> 16) & 0xFF, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: RegLong, r: RegLong) => new VisitArray(0x1A, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        }
      case And(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitArray(0xE0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
          case (l: RegByte, r: RegByte) => new VisitArray(0x16, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegWord) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x79, 0x60 | visit(r).asInstanceOf[VisitInt].item, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: RegWord, r: RegWord) => new VisitArray(0x66, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegLong) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x7A, 0x60 | visit(r).asInstanceOf[VisitInt].item, (imm >> 24) & 0xFF, (imm >> 16) & 0xFF, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: RegLong, r: RegLong) => new VisitArray(0x01, 0xF0, 0x66, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        }
      case Not(item) =>
        item match {
          case i: RegByte => new VisitArray(0x17, visit(i).asInstanceOf[VisitInt].item)
          case i: RegWord => new VisitArray(0x17, 0x10 | visit(i).asInstanceOf[VisitInt].item)
          case i: RegLong => new VisitArray(0x17, 0x30 | visit(i).asInstanceOf[VisitInt].item)
        }
      case Andc(item) => new VisitArray(0x06, visit(item).asInstanceOf[VisitInt].item)
      case Orc(item) => new VisitArray(0x04, visit(item).asInstanceOf[VisitInt].item)
      case Mov(left, right) =>
        (left, right) match {
          case (l: RegByte, r: RegByte) => new VisitArray(0x0C, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: RegWord, r: RegWord) => new VisitArray(0x0D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: RegLong, r: RegLong) => new VisitArray(0x0F, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: RegByte) => new VisitArray(0xF0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
          case (l: IndirReg, r: RegByte) => new VisitArray(0x68, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Disp, r: RegByte) =>
            val disp = visit(l).asInstanceOf[VisitArray].item
            if (disp(2) == 16) new VisitArray(0x6E, (disp(1) << 4) | visit(r).asInstanceOf[VisitInt].item, (disp(1) >> 8) & 0xFF, disp(1) & 0xFF)
            else new VisitArray(0x78, disp(1) << 4, 0x6A, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, (disp(0) >> 16) & 0xFF, (disp(0) >> 8) & 0xFF, disp(0) & 0xFF)
          case (l: Pos, r: RegByte) => new VisitArray(0x6C, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: AbsAddress, r: RegByte) =>
            val abs = visit(l).asInstanceOf[VisitArray].item
            if (abs(1) == 8) new VisitArray(0x20 | visit(r).asInstanceOf[VisitInt].item, abs(0))
            else if (abs(1) == 16) new VisitArray(0x6A, visit(r).asInstanceOf[VisitInt].item, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
            else new VisitArray(0x6A, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, (abs(0) >> 16) & 0xFF, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
          case (l: Imm, r: RegWord) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x79, visit(r).asInstanceOf[VisitInt].item, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: IndirReg, r: RegWord) => new VisitArray(0x69, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Disp, r: RegWord) =>
            val disp = visit(l).asInstanceOf[VisitArray].item
            if (disp(2) == 16) new VisitArray(0x6F, (disp(1) << 4) | visit(r).asInstanceOf[VisitInt].item, (disp(1) >> 8) & 0xFF, disp(1) & 0xFF)
            else new VisitArray(0x78, disp(1) << 4, 0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, (disp(0) >> 16) & 0xFF, (disp(0) >> 8) & 0xFF, disp(0) & 0xFF)
          case (l: Pos, r: RegWord) => new VisitArray(0x6D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: AbsAddress, r: RegWord) =>
            val abs = visit(l).asInstanceOf[VisitArray].item
            if (abs(1) == 16) new VisitArray(0x6B, visit(r).asInstanceOf[VisitInt].item, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
            else new VisitArray(0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, (abs(0) >> 16) & 0xFF, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
          case (l: Imm, r: RegLong) =>
            val imm = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(0x7A, visit(r).asInstanceOf[VisitInt].item, (imm >> 24) & 0xFF, (imm >> 16) & 0xFF, (imm >> 8) & 0xFF, imm & 0xFF)
          case (l: IndirReg, r: RegLong) => new VisitArray(0x01, 0x00, 0x69, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Disp, r: RegLong) =>
            val disp = visit(l).asInstanceOf[VisitArray].item
            if (disp(2) == 16) new VisitArray(0x01, 0x00, 0x6F, (disp(1) << 4) | visit(r).asInstanceOf[VisitInt].item, (disp(1) >> 8) & 0xFF, disp(1) & 0xFF)
            else new VisitArray(0x01, 0x00, 0x78, disp(1) << 4, 0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, (disp(0) >> 16) & 0xFF, (disp(0) >> 8) & 0xFF, disp(0) & 0xFF)
          case (l: Pos, r: RegLong) => new VisitArray(0x01, 0x00, 0x6D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: AbsAddress, r: RegLong) =>
            val abs = visit(l).asInstanceOf[VisitArray].item
            if (abs(1) == 16) new VisitArray(0x01, 0x00, 0x6B, visit(r).asInstanceOf[VisitInt].item, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
            else new VisitArray(0x01, 0x00, 0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, (abs(0) >> 16) & 0xFF, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
          case (l: RegByte, r: IndirReg) => new VisitArray(0x68, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
          case (l: RegByte, r: Disp) =>
            val disp = visit(r).asInstanceOf[VisitArray].item
            if (disp(2) == 16) new VisitArray(0x6E, 0x80 | (disp(1) << 4) | visit(l).asInstanceOf[VisitInt].item, (disp(1) >> 8) & 0xFF, disp(1) & 0xFF)
            else new VisitArray(0x78, disp(1) << 4, 0x6A, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, (disp(0) >> 16) & 0xFF, (disp(0) >> 8) & 0xFF, disp(0) & 0xFF)
          case (l: RegByte, r: Pre) => new VisitArray(0x6C, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
          case (l: RegByte, r: AbsAddress) =>
            val abs = visit(r).asInstanceOf[VisitArray].item
            if (abs(1) == 8) new VisitArray(0x30 | visit(l).asInstanceOf[VisitInt].item, abs(0))
            else if (abs(1) == 16) new VisitArray(0x6A, 0x80 | visit(l).asInstanceOf[VisitInt].item, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
            else new VisitArray(0x6A, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, (abs(0) >> 16) & 0xFF, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
          case (l: RegWord, r: IndirReg) => new VisitArray(0x69, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
          case (l: RegWord, r: Disp) =>
            val disp = visit(r).asInstanceOf[VisitArray].item
            if (disp(2) == 16) new VisitArray(0x6F, 0x80 | (disp(1) << 4) | visit(l).asInstanceOf[VisitInt].item, (disp(1) >> 8) & 0xFF, disp(1) & 0xFF)
            else new VisitArray(0x78, disp(1) << 4, 0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, (disp(0) >> 16) & 0xFF, (disp(0) >> 8) & 0xFF, disp(0) & 0xFF)
          case (l: RegWord, r: Pre) => new VisitArray(0x6D, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
          case (l: RegWord, r: AbsAddress) =>
            val abs = visit(r).asInstanceOf[VisitArray].item
            if (abs(1) == 16) new VisitArray(0x6B, 0x80 | visit(l).asInstanceOf[VisitInt].item, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
            else new VisitArray(0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, (abs(0) >> 16) & 0xFF, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
          case (l: RegLong, r: IndirReg) => new VisitArray(0x01, 0x00, 0x69, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
          case (l: RegLong, r: Disp) =>
            val disp = visit(r).asInstanceOf[VisitArray].item
            if (disp(2) == 16) new VisitArray(0x01, 0x00, 0x6F, 0x80 | (disp(1) << 4) | visit(l).asInstanceOf[VisitInt].item, (disp(1) >> 8) & 0xFF, disp(1) & 0xFF)
            else new VisitArray(0x01, 0x00, 0x78, 0x80 | (disp(1) << 4), 0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, (disp(0) >> 16) & 0xFF, (disp(0) >> 8) & 0xFF, disp(0) & 0xFF)
          case (l: RegLong, r: Pre) => new VisitArray(0x01, 0x00, 0x6D, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
          case (l: RegLong, r: AbsAddress) =>
            val abs = visit(r).asInstanceOf[VisitArray].item
            if (abs(1) == 16) new VisitArray(0x01, 0x00, 0x6B, 0x80 | visit(l).asInstanceOf[VisitInt].item, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
            else new VisitArray(0x01, 0x00, 0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, (abs(0) >> 16) & 0xFF, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
        }
      case Pop(reg) =>
        reg match {
          case (r: RegWord) => new VisitArray(0x6D, 0x70 | visit(r).asInstanceOf[VisitInt].item)
          case (r: RegLong) => new VisitArray(0x01, 0x00, 0x6D, 0x70 | visit(r).asInstanceOf[VisitInt].item)
        }
      case Push(reg) =>
        reg match {
          case (r: RegWord) => new VisitArray(0x6D, 0xF0 | visit(r).asInstanceOf[VisitInt].item)
          case (r: RegLong) => new VisitArray(0x01, 0x00, 0x6D, 0xF0 | visit(r).asInstanceOf[VisitInt].item)
        }
      case Extu(reg) =>
        reg match {
          case (r: RegWord) => new VisitArray(0x17, 0x50 | visit(r).asInstanceOf[VisitInt].item)
          case (r: RegLong) => new VisitArray(0x17, 0x70 | visit(r).asInstanceOf[VisitInt].item)
        }
      case Bclr(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitArray(0x72, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x72, visit(l).asInstanceOf[VisitInt].item << 4)
          case (l: Imm, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x72, visit(l).asInstanceOf[VisitInt].item << 4)
          case (l: RegByte, r: RegByte) => new VisitArray(0x62, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: RegByte, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x62, visit(l).asInstanceOf[VisitInt].item << 4)
          case (l: RegByte, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x62, visit(l).asInstanceOf[VisitInt].item << 4)
        }
      case Bset(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitArray(0x70, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: Imm, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x70, visit(l).asInstanceOf[VisitInt].item << 4)
          case (l: Imm, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x70, visit(l).asInstanceOf[VisitInt].item << 4)
          case (l: RegByte, r: RegByte) => new VisitArray(0x60, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
          case (l: RegByte, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x60, visit(l).asInstanceOf[VisitInt].item << 4)
          case (l: RegByte, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x60, visit(l).asInstanceOf[VisitInt].item << 4)
        }
      case Jmp(add) =>
        add match {
          case (a: IndirReg) => new VisitArray(0x59, visit(a).asInstanceOf[VisitInt].item << 4)
          case (a: AbsAddress) =>
            val abs = visit(a).asInstanceOf[VisitArray].item
            new VisitArray(0x5A, (abs(0) >> 16) & 0xFF, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
          case (a: IndirAdd) => new VisitArray(0x5B, visit(a).asInstanceOf[VisitInt].item)
        }
      case Jsr(add) =>
        add match {
          case (a: IndirReg) => new VisitArray(0x5D, visit(a).asInstanceOf[VisitInt].item << 4)
          case (a: AbsAddress) =>
            val abs = visit(a).asInstanceOf[VisitArray].item
            new VisitArray(0x5E, (abs(0) >> 16) & 0xFF, (abs(0) >> 8) & 0xFF, abs(0) & 0xFF)
          case (a: IndirAdd) => new VisitArray(0x5F, visit(a).asInstanceOf[VisitInt].item)
        }
      case Bra(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        if (size == 8) new VisitArray(0x40, disp)
        else new VisitArray(0x58, 0x00, (disp >> 8) & 0xFF, disp & 0xFF)
      case Blo(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        if (size == 8) new VisitArray(0x45, disp)
        else new VisitArray(0x58, 0x50, (disp >> 8) & 0xFF, disp & 0xFF)
      case Blt(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        if (size == 8) new VisitArray(0x4D, disp)
        else new VisitArray(0x58, 0xD0, (disp >> 8) & 0xFF, disp & 0xFF)
      case Bhi(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        if (size == 8) new VisitArray(0x42, disp)
        else new VisitArray(0x58, 0x20, (disp >> 8) & 0xFF, disp & 0xFF)
      case Rts() => new VisitArray(0x54, 0x70)
      case Rte() => new VisitArray(0x56, 0x70)
      case Data(num, size) =>
        size match {
          case 8 => new VisitArray(visit(num).asInstanceOf[VisitInt].item)
          case 16 =>
            val data = visit(num).asInstanceOf[VisitInt].item
            new VisitArray((data >> 8) & 0xFF, data & 0xFF)
          case 32 =>
            val data = visit(num).asInstanceOf[VisitInt].item
            new VisitArray((data >> 24) & 0xFF, (data >> 16) & 0xFF, (data >> 8) & 0xFF, data & 0xFF)
        }
      case DataBlock(block, data, size) =>
        size match {
          case 8 => new VisitArray(Array.fill(visit(block).asInstanceOf[VisitInt].item)(visit(data).asInstanceOf[VisitInt].item))
          case 16 =>
            val d = visit(data).asInstanceOf[VisitInt].item
            val array = new Array[Int](2 * visit(block).asInstanceOf[VisitInt].item)
            for (i <- 0 until array.length / 2) {
              array(i) = (d >> 8) & 0xFF
              array(i + 1) = d & 0xFF
            }
            new VisitArray(array)
          case 32 =>
            val d = visit(data).asInstanceOf[VisitInt].item
            val array = new Array[Int](4 * visit(block).asInstanceOf[VisitInt].item)
            for (i <- 0 until array.length / 4) {
              array(i) = (d >> 24) & 0xFF
              array(i + 1) = (d >> 16) & 0xFF
              array(i + 2) = (d >> 8) & 0xFF
              array(i + 3) = d & 0xFF
            }
            new VisitArray(array)
        }
      case LabelName(num) => new VisitInt(label(num))
      case MakeLabel(num) => new VisitInt(0)
      case Imm(num) => visit(num)
      case AbsAddress(num, size) => new VisitArray(visit(num).asInstanceOf[VisitInt].item, size)
      case IndirReg(reg) => visit(reg)
      case IndirAdd(add) => visit(add)
      case Disp(disp, reg, size) => new VisitArray(visit(disp).asInstanceOf[VisitInt].item, visit(reg).asInstanceOf[VisitInt].item, size)
      case Pos(item) => visit(item)
      case Pre(item) => visit(item)
      case Section(sec) =>
        section = sec
        new VisitInt(0)
      case RegByte(num) => new VisitInt(num)
      case RegWord(num) => new VisitInt(num)
      case RegLong(num) => new VisitInt(num)
      case Number(num) => new VisitInt(num)
      case Minus(num) => new VisitInt(-visit(num).asInstanceOf[VisitInt].item)
      case Rev(num) => new VisitInt(~visit(num).asInstanceOf[VisitInt].item)
      case High(num) => new VisitInt((visit(num).asInstanceOf[VisitInt].item >> 8) & 0xFF)
      case Low(num) => new VisitInt(visit(num).asInstanceOf[VisitInt].item & 0xFF)
      case HWord(num) => new VisitInt((visit(num).asInstanceOf[VisitInt].item >> 8) & 0xFFFF)
      case LWord(num) => new VisitInt(visit(num).asInstanceOf[VisitInt].item & 0xFFFF)
      case StartOf(sec) => new VisitInt(Parameter.start(sec))
      case SizeOf(sec) => new VisitInt(count(sec))
      case Expr(op, left, right) =>
        op match {
          case "+" => new VisitInt(visit(left).asInstanceOf[VisitInt].item + visit(right).asInstanceOf[VisitInt].item)
          case "-" => new VisitInt(visit(left).asInstanceOf[VisitInt].item - visit(right).asInstanceOf[VisitInt].item)
          case "*" => new VisitInt(visit(left).asInstanceOf[VisitInt].item * visit(right).asInstanceOf[VisitInt].item)
          case "/" => new VisitInt(visit(left).asInstanceOf[VisitInt].item / visit(right).asInstanceOf[VisitInt].item)
          case "&" => new VisitInt(visit(left).asInstanceOf[VisitInt].item & visit(right).asInstanceOf[VisitInt].item)
          case "|" => new VisitInt(visit(left).asInstanceOf[VisitInt].item | visit(right).asInstanceOf[VisitInt].item)
          case "~" => new VisitInt(visit(left).asInstanceOf[VisitInt].item ^ visit(right).asInstanceOf[VisitInt].item)
          case ">>" => new VisitInt(visit(left).asInstanceOf[VisitInt].item >> visit(right).asInstanceOf[VisitInt].item)
          case "<<" => new VisitInt(visit(left).asInstanceOf[VisitInt].item << visit(right).asInstanceOf[VisitInt].item)
        }
    }
  }

}

trait Visit {
  type T
  val item: T
}

class VisitInt(num: Int) extends Visit {
  type T = Int
  val item = num
}

class VisitArray(num: Array[Int]) extends Visit {
  type T = Array[Int]
  val item = num

  def this(num: Int*) = this(num.toArray)
}

class VisitString(num: String) extends Visit {
  type T = String
  val item = num
}

class MyHashMap extends mutable.HashMap[String, Int] {

  override def put(key: String, value: Int): Option[Int] = {
    key match {
      case "V" =>
        super.put("V", countup("V", value))
      case "P" =>
        super.put("P", countup("P", value))
        super.put("C", countup("C", value))
        super.put("D", countup("D", value))
      case "C" =>
        super.put("C", countup("C", value))
        super.put("D", countup("D", value))
      case "D" =>
        super.put("D", countup("D", value))
      case "B" =>
        super.put("B", countup("B", value))
        super.put("R", countup("R", value))
      case "R" =>
        super.put("R", countup("R", value))
    }
  }

  def countup(key: String, value: Int): Int = super.get(key).get + value
}

