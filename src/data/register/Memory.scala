package data.register

import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/17.
 */
class Memory(c: Z3Context, m: mutable.HashMap[Int, Z3AST]) {

  val mem = m
  private final val ctx = c
  private final val div = 4
  private final val limit = 0xFFFF / div
  private final val bv32 = ctx.mkBVSort(32)

  def address(num: Int): (Int, Int) = ((num / div) & limit) -> ((num % div) & limit)

  def getByte(num: Int): Z3AST = {
    val p = address(num)
    ctx.mkBVAnd(ctx.mkBVLshr(mem(p._1), ctx.mkInt({
      if (p._2 == 0) 24
      else if (p._2 == 1) 16
      else if (p._2 == 2) 8
      else 0
    }, bv32)), ctx.mkInt(0x000000FF, bv32))
  }

  def getWord(num: Int): Z3AST = {
    val p = address(num)
    ctx.mkBVAnd(ctx.mkBVLshr(mem(p._1), ctx.mkInt({
      if (p._2 == 0) 16
      else 0
    }, bv32)), ctx.mkInt(0x0000FFFF, bv32))
  }

  def getLong(num: Int): Z3AST = mem((num / 4) & limit)

  def setByte(data: Z3AST, num: Int): Unit = {
    val p = address(num)
    val m = ctx.mkBVAnd(mem(p._1), ctx.mkInt({
      if (p._2 == 0) 0x00FFFFFF
      else if (p._2 == 1) 0xFF00FFFF
      else if (p._2 == 2) 0xFFFF00FF
      else 0xFFFFFF00
    }, bv32))
    val d = ctx.mkBVShl(ctx.mkAnd(data, ctx.mkInt(0x000000FF, bv32)), ctx.mkInt({
      if (p._2 == 0) 24
      else if (p._2 == 1) 16
      else if (p._2 == 2) 8
      else 0
    }, bv32))
    mem(p._1) = ctx.mkBVOr(m, d)
  }

  def setWord(data: Z3AST, num: Int): Unit = {
    val p = address(num)
    val m = ctx.mkBVAnd(mem(p._1), ctx.mkInt({
      if (p._2 == 0) 0x0000FFFF
      else 0xFFFF0000
    }, bv32))
    val d = ctx.mkBVShl(ctx.mkAnd(data, ctx.mkInt(0x000000FF, bv32)), ctx.mkInt({
      if (p._2 == 0) 16
      else 0
    }, bv32))
    mem(p._1) = ctx.mkBVOr(m, d)
  }

  def setLong(data: Z3AST, num: Int): Unit = mem(num & limit) = data
}
