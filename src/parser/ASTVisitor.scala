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

  def makeProgram(ctx: Z3Context, file: File): Memory = {
    file.listFiles.foreach { f =>
      println(f)
      Source.fromFile(f).getLines.foreach(l => println(new ASTParser().parse(l)))
    }
    new Memory(ctx, new mutable.HashMap[Int, Z3AST]())
  }

  //とりあえず置いといて
  def visit(ast: AST): VisitItem = {
    ast match {
      case Add(left, right) =>
        (left, right) match {
          case (l: Imm, r: RegByte) => new VisitArray(Array(0x80 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item))
          case (l: RegByte, r: RegByte) => new VisitArray(Array(0x08, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item))
          case (l: Imm, r: RegWord) =>
            val one = 0x79
            val two = 0x10 | visit(r).asInstanceOf[VisitInt].item
            val hold = visit(l).asInstanceOf[VisitInt].item
            new VisitArray(Array(one, two, hold << 8, hold & 0xFF))
        }
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
