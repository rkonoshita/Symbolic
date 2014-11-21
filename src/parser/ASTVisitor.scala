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
  val count = new mutable.HashMap[String, Int]
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

  def search(ast: AST): Int = {
    ast match {
      case Add(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => 2
          case (l: RegByte, r: RegByte) => 2
          case (l: Imm, r: RegWord) => 4
          case (l: RegWord, r: RegWord) => 2
          case (l: Imm, r: RegLong) => 6
          case (l: RegLong, r: RegLong) => 2
        }
      case MakeLabel(str) => label += str -> count(section)
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

