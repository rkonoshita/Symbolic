package data.register

import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
class Register(c: Z3Context, r: mutable.HashMap[Int, Z3AST]) {

  val reg = r
  private val ctx = c
  private val limit = 0x07
  private val bv32 = ctx.mkBVSort(32)

  //レジスタへのアクセスが記号で行われた場合
  def getByte(num: Z3AST): ArrayBuffer[Z3AST] = {
    val s = ctx.mkSolver
    val array = new ArrayBuffer[Z3AST]
    //記号が0~fのどれに合致するか検査
    (0 to 0x0f).foreach { n =>
      s.push //push
      s.assertCnstr(ctx.mkEq(ctx.mkBVAnd(num, ctx.mkInt(0x0F, bv32)), ctx.mkInt(n, bv32)))
      if (s.check.get) array += getByte(n)
      s.pop(1) //assertをpop
    }
    array
  }

  def getByte(num: Int): Z3AST =
    if ((num & 0x8) == 0x8) getByteLow(num)
    else getByteHigh(num)

  private def getByteHigh(num: Int): Z3AST = ctx.mkBVAnd(ctx.mkBVAshr(reg(num & limit), ctx.mkInt(8, bv32)), ctx.mkInt(0x000000FF, bv32))

  private def getByteLow(num: Int): Z3AST = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0x000000FF, bv32))

  //レジスタへのアクセスが記号で行われた場合
  def getWord(num: Z3AST): ArrayBuffer[Z3AST] = {
    val s = ctx.mkSolver
    val array = new ArrayBuffer[Z3AST]
    //記号が0~fのどれに合致するか検査
    (0 to 0xf).foreach { n =>
      s.push //push
      s.assertCnstr(ctx.mkEq(ctx.mkBVAnd(num, ctx.mkInt(0x0000000F, bv32)), ctx.mkInt(n, bv32)))
      if (s.check.get) array += getWord(n)
      s.pop(1) //assertをpop
    }
    array
  }

  def getWord(num: Int): Z3AST =
    if ((num & 0x8) == 0x8) getWordHigh(num)
    else getWordLow(num)

  private def getWordHigh(num: Int): Z3AST = ctx.mkBVAnd(ctx.mkBVAshr(reg(num & limit), ctx.mkInt(16, bv32)), ctx.mkInt(0x0000FFFF, bv32))

  private def getWordLow(num: Int): Z3AST = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0x0000FFFF, bv32))

  //レジスタへのアクセスが記号で行われた場合
  def getLong(num: Z3AST): ArrayBuffer[Z3AST] = {
    val s = ctx.mkSolver
    val array = new ArrayBuffer[Z3AST]
    //記号が0~fのどれに合致するか検査
    (0 to 7).foreach { n =>
      s.push //push
      s.assertCnstr(ctx.mkEq(ctx.mkBVAnd(num, ctx.mkInt(0x00000007, bv32)), ctx.mkInt(n, bv32)))
      if (s.check.get) array += getLong(n)
      s.pop(1) //assertをpop
    }
    array
  }

  def getLong(num: Int): Z3AST = reg(num & limit)

  def setByte(data: Z3AST, num: Int): Unit =
    if ((num & 0x8) == 0x8) setByteLow(data, num)
    else setByteHigh(data, num)

  private def setByteHigh(data: Z3AST, num: Int): Unit = {
    val r = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0xFFFF00FF, bv32))
    val d = ctx.mkBVShl(ctx.mkBVAnd(data, ctx.mkInt(0x000000FF, bv32)), ctx.mkInt(8, bv32))
    reg(num & limit) = ctx.mkBVOr(r, d)
  }

  private def setByteLow(data: Z3AST, num: Int): Unit = {
    val r = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0xFFFFFF00, bv32))
    val d = ctx.mkBVAnd(data, ctx.mkInt(0x000000FF, bv32))
    reg(num & limit) = ctx.mkBVOr(r, d)
  }

  def setWord(data: Z3AST, num: Int): Unit = {
    if ((num & 0x8) == 0x8) setWordHigh(data, num)
    else setWordLow(data, num)
  }

  private def setWordHigh(data: Z3AST, num: Int): Unit = {
    val r = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0x0000FFFF, bv32))
    val d = ctx.mkBVShl(ctx.mkBVAnd(data, ctx.mkInt(0x0000FFFF, bv32)), ctx.mkInt(16, bv32))
    reg(num & limit) = ctx.mkBVOr(r, d)
  }

  private def setWordLow(data: Z3AST, num: Int): Unit = {
    val r = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0xFFFF0000, bv32))
    val d = ctx.mkBVAnd(data, ctx.mkInt(0x0000FFFF, bv32))
    reg(num & limit) = ctx.mkBVOr(r, d)
  }

  def setLong(data: Z3AST, num: Int): Unit = reg(num & limit) = data
}
