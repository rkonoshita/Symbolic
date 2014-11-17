package data.register

import forZ3.{ExprVisitor, ParserForZ3}
import z3.scala.{Z3Context, Z3AST}

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/12.
 */
class Register(r: mutable.HashMap[Int, Z3AST]) {

  val reg = r
  val ctx = new Z3Context
  val par = new ParserForZ3

  def getByte(num: Int): Z3AST = {
    if ((num & 0x80) == 0x80) getByteLow(num)
    else getByteHigh(num)
  }

  private def getByteHigh(num: Int): Z3AST = {
    val d = ctx.mkBVAshr(reg(num & 0x07), ctx.mkInt(8, ctx.mkBVSort(32)))
    val p = par.parse(d.toString)
    val vis = new ExprVisitor(8)
    vis.visit(p)
  }

  private def getByteLow(num: Int): Z3AST = {
    val d = reg(num & 0x07)
    val p = par.parse(d.toString)
    val vis = new ExprVisitor(8)
    vis.visit(p)
  }

  def getWord(num: Int): Z3AST = {
    if ((num & 0x80) == 0x80) getWordHigh(num)
    else getWordLow(num)
  }

  private def getWordHigh(num: Int): Z3AST = {
    val d = ctx.mkBVAshr(reg(num & 0x07), ctx.mkInt(16, ctx.mkBVSort(32)))
    val p = par.parse(d.toString)
    val vis = new ExprVisitor(16)
    vis.visit(p)
  }

  private def getWordLow(num: Int): Z3AST = {
    val d = reg(num & 0x07)
    val p = par.parse(d.toString)
    val vis = new ExprVisitor(16)
    vis.visit(p)
  }

  def getLong(num: Int): Z3AST = reg(num & 0x07)

  def setByte(data: Z3AST, num: Int): Unit = {

  }
}
