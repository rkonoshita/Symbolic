package data.register

import data.DataSet
import z3.scala.{Z3Context, Z3AST}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by ryosuke on 14/11/18.
 */
class ProgramCounter(c: Z3Context, p: Z3AST) {

  var pc = p
  private val ctx = c
  private val limit = 0x0000FFFF
  private val bv32 = ctx.mkBVSort(32)

  def getPC: ArrayBuffer[Int] = {
    val array = new ArrayBuffer[Int]
    val s = ctx.mkSolver
    (0 to limit).foreach { p =>
      s.push
      s.assertCnstr(ctx.mkEq(pc, ctx.mkInt(p, bv32)))
      if (s.check.get) array += p
      s.pop(1)
    }
    array
  }

  def setPC(p: Z3AST): Unit = pc = ctx.mkBVAnd(p, ctx.mkInt(limit, bv32))
}
