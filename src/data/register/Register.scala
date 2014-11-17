package data.register

import forZ3.{ExprVisitor, ParserForZ3}
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
class Register(c: Z3Context, r: mutable.HashMap[Int, Z3AST]) {

  val reg = r
  private final val ctx = c
  private final val limit = 0x07
  private final val bv32 = ctx.mkBVSort(32)

//  def getByte(num: Int): Z3AST = {
//    var list = List.empty[Z3AST]
//    val vis = new ExprVisitor(ctx, 8)
//    new ParserForZ3().parse {
//      if ((num & 0x8) == 0x8) getByteLow(num)
//      else getByteHigh(num)
//    }.foreach(l => list = vis.visit(l) :: list)
//    list.head
//  }

  def getByte(num: Int): Z3AST = {
    new ExprVisitor(ctx, 8).visit(new ParserForZ3().parse {
      if ((num & 0x8) == 0x8) getByteLow(num)
      else getByteHigh(num)
    })
  }

  private def getByteHigh(num: Int): String = ctx.mkBVAshr(reg(num & limit), ctx.mkInt(8, bv32)).toString

  private def getByteLow(num: Int): String = reg(num & limit).toString

//    def getWord(num: Int): Z3AST = {
//      var list = List.empty[Z3AST]
//      val vis = new ExprVisitor(ctx, 16)
//      new ParserForZ3().parse {
//        if ((num & 0x8) == 0x8) getWordHigh(num)
//        else getWordLow(num)
//      }.foreach(l => list = vis.visit(l) :: list)
//      list.head
//    }

  def getWord(num: Int): Z3AST = {
    new ExprVisitor(ctx, 16).visit(new ParserForZ3().parse {
      if ((num & 0x8) == 0x8) getWordHigh(num)
      else getWordLow(num)
    })
  }

  private def getWordHigh(num: Int): String = ctx.mkBVAshr(reg(num & limit), ctx.mkInt(16, bv32)).toString

  private def getWordLow(num: Int): String = reg(num & limit).toString

  def getLong(num: Int): Z3AST = reg(num & limit)

//    def setByte(data: Z3AST, num: Int): Unit = {
//      var list = List.empty[Z3AST]
//      val vis = new ExprVisitor(ctx, 32)
//      new ParserForZ3().parse(data.toString).foreach(l => list = vis.visit(l) :: list)
//      val d =
//        if ((num & 0x8) == 0x8) setByteLow(list.head, num)
//        else setByteHigh(list.head, num)
//      reg(num & limit) = ctx.mkBVOr(d._1, d._2)
//    }

  def setByte(data: Z3AST, num: Int): Unit = {
    val vis = new ExprVisitor(ctx, 32).visit(new ParserForZ3().parse(data.toString))
    val d =
      if ((num & 0x8) == 0x8) setByteLow(vis, num)
      else setByteHigh(vis, num)
    reg(num & limit) = ctx.mkBVOr(d._1, d._2)
  }

  private def setByteHigh(data: Z3AST, num: Int): (Z3AST, Z3AST) = {
    val r = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0xFFFF00FF, bv32))
    val d = ctx.mkBVShl(ctx.mkBVAnd(data, ctx.mkInt(0x000000FF, bv32)), ctx.mkInt(8, bv32))
    (r, d)
  }

  private def setByteLow(data: Z3AST, num: Int): (Z3AST, Z3AST) = {
    val r = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0xFFFFFF00, bv32))
    val d = ctx.mkBVAnd(data, ctx.mkInt(0x000000FF, bv32))
    (r, d)
  }

//  def setWord(data: Z3AST, num: Int): Unit = {
//    var list = List.empty[Z3AST]
//    val vis = new ExprVisitor(ctx, 32)
//    new ParserForZ3().parse(data.toString).foreach(l => list = vis.visit(l) :: list)
//    val d =
//      if ((num & 0x8) == 0x8) setWordHigh(list.head, num)
//      else setWordLow(list.head, num)
//    ctx.mkBVOr(d._1, d._2)
//  }

  def setWord(data: Z3AST, num: Int): Unit = {
    val vis = new ExprVisitor(ctx, 32).visit(new ParserForZ3().parse(data.toString))
    val d =
      if ((num & 0x8) == 0x8) setWordHigh(vis, num)
      else setWordLow(vis, num)
    reg(num & limit) = ctx.mkBVOr(d._1, d._2)
  }

  private def setWordHigh(data: Z3AST, num: Int): (Z3AST, Z3AST) = {
    val r = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0x0000FFFF, bv32))
    val d = ctx.mkBVShl(ctx.mkBVAnd(data, ctx.mkInt(0x0000FFFF, bv32)), ctx.mkInt(16, bv32))
    (r, d)
  }

  private def setWordLow(data: Z3AST, num: Int): (Z3AST, Z3AST) = {
    val r = ctx.mkBVAnd(reg(num & limit), ctx.mkInt(0x0000FFFF, bv32))
    val d = ctx.mkBVShl(ctx.mkBVAnd(data, ctx.mkInt(0x0000FFFF, bv32)), ctx.mkInt(16, bv32))
    (r, d)
  }

  def setLong(data: Z3AST, num: Int): Unit = reg(num & limit) = data
}
