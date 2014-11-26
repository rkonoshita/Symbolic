package main

import java.io.File

import parser.ASTVisitor
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
object Main {

  val ctx = new Z3Context
  var symnum = -1

  def main(args: Array[String]): Unit = {
    val file = new File("target") -> new File("asm")
    new ConvertToInputForm(file._1, file._2).convert()
    new ASTVisitor().makeProgram(ctx, file._2)
  }

  def makeSymbol(size: Int): Z3AST = {
    symnum += 1
    ctx.mkConst("s" + symnum, ctx.mkBVSort(size))
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
