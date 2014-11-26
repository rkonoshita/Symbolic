package data.register

import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by ryosuke on 14/11/18.
 */
class ConditionRegister(ct: Z3Context, c: MySymbol) {

  var ccr = c
  private val ctx = ct

  def clearV: MySymbol =
    ccr.symbol match {
      case d: Int => new IntSymbol(d & 0xFD)
      case d: Z3AST => new CtxSymbol(ctx.mkBVAnd(d, ctx.mkInt(0xFD, d.getSort)))
    }

  def clearZ: MySymbol =
    ccr.symbol match {
      case d: Int => new IntSymbol(d & 0xFB)
      case d: Z3AST => new CtxSymbol(ctx.mkBVAnd(d, ctx.mkInt(0xFB, d.getSort)))
    }

  def clearN: MySymbol =
    ccr.symbol match {
      case d: Int => new IntSymbol(d & 0xF7)
      case d: Z3AST => new CtxSymbol(ctx.mkBVAnd(d, ctx.mkInt(0xF7, d.getSort)))
    }

  def setV: MySymbol =
    ccr.symbol match {
      case d: Int => new IntSymbol(d | 0x02)
      case d: Z3AST => new CtxSymbol(ctx.mkBVOr(d, ctx.mkInt(0x02, d.getSort)))
    }

  def setZ: MySymbol =
    ccr.symbol match {
      case d: Int => new IntSymbol(d | 0x04)
      case d: Z3AST => new CtxSymbol(ctx.mkBVOr(d, ctx.mkInt(0x04, d.getSort)))
    }

  def setN: MySymbol =
    ccr.symbol match {
      case d: Int => new IntSymbol(d | 0x08)
      case d: Z3AST => new CtxSymbol(ctx.mkBVOr(d, ctx.mkInt(0x08, d.getSort)))
    }

  def checkZ(data: MySymbol): ArrayBuffer[MySymbol] = {
    val buf = new ArrayBuffer[MySymbol]
    data.symbol match {
      case d: Int => buf += (if (d == 0) setZ else clearZ)
      case d: Z3AST =>
        val s = ctx.mkSolver
        s.push
        val eq = ctx.mkEq(d, ctx.mkInt(0, d.getSort))
        s.assertCnstr(eq)
        val b1 = s.check.get
        s.pop(1)
        s.assertCnstr(ctx.mkNot(eq))
        val b2 = s.check.get
        if (b1 & b2 == true) {
          buf += setZ
          buf += clearZ
        }
        else if (b1 == true) buf += setZ
        else buf += clearZ
    }
    buf
  }

  def checkN(data: MySymbol): ArrayBuffer[MySymbol] = {
    val buf = new ArrayBuffer[MySymbol]
    data.symbol match {
      case d: Int => buf += (if (d == 0) setN else clearN)
      case d: Z3AST =>
        val s = ctx.mkSolver
        s.push
        val eq = ctx.mkBVSge(d, ctx.mkInt(0, d.getSort))
        s.assertCnstr(eq)
        val b1 = s.check.get
        s.pop(1)
        s.assertCnstr(ctx.mkNot(eq))
        val b2 = s.check.get
        if (b1 & b2 == true) {
          buf += setN
          buf += clearN
        }
        else if (b1 == true) buf += setN
        else buf += clearN
    }
    buf
  }

}
