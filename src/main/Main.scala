package main

import java.io.File

import data.register.{Register, Memory}
import parser.ASTVisitor
import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
object Main {

  val ctx = new Z3Context
  var symnum = -1

  def main(args: Array[String]): Unit = {
    val m = new Memory(ctx, new mutable.HashMap[Int, MySymbol])
    val r = new Register(ctx, new mutable.HashMap[Int, MySymbol])
    println(m.getWord(0))
    println(m.getLong(0))
//    val file = new File("target") -> new File("asm")
//    new ConvertToInputForm(file._1, file._2).convert()
//    new ASTVisitor().makeProgram(ctx, file._2)
  }

  def makeSymbol: Z3AST = {
    symnum += 1
    ctx.mkConst("s" + symnum, ctx.mkBVSort(32))
  }

  def extract(range: Range.Inclusive, ast: Z3AST): ArrayBuffer[Int] = {
    val s = ctx.mkSolver
    val buf = new ArrayBuffer[Int]
    range.foreach { n =>
      s.push
      s.assertCnstr(ctx.mkEq(ast, ctx.mkInt(n, ast.getSort)))
      if (s.check.get) buf += n
      s.pop(1)
    }
    buf
  }

}
