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
    ccr match {
      case d: IntSymbol => d & 0xFD
      case d: CtxSymbol => d & 0xFD
    }

  def clearZ: MySymbol =
    ccr match {
      case d: IntSymbol => d & 0xFB
      case d: CtxSymbol => d & 0xFB
    }

  def clearN: MySymbol =
    ccr match {
      case d: IntSymbol => d & 0xF7
      case d: CtxSymbol => d & 0xF7
    }

  def setV: MySymbol =
    ccr match {
      case d: IntSymbol => d | 0x02
      case d: CtxSymbol => d | 0x02
    }

  def setZ: MySymbol =
    ccr match {
      case d: IntSymbol => d | 0x04
      case d: CtxSymbol => d | 0x04
    }

  def setN: MySymbol =
    ccr match {
      case d: IntSymbol => d | 0x08
      case d: CtxSymbol => d | 0x08
    }

  def checkZ(data: MySymbol, size: Int): ArrayBuffer[MySymbol] = {
    val buf = new ArrayBuffer[MySymbol]
    val and = size match {
      case 8 => 0xFF
      case 16 => 0xFFFF
      case 32 => 0xFFFFFFFF
    }
    data match {
      case d: IntSymbol =>
        buf += (if ((d & and).symbol == 0) setZ else clearZ)
      case d: CtxSymbol =>
        val s = ctx.mkSolver
        s.push
        val eq = d.&(and).eq(0).symbol
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

  def checkN(data: MySymbol, size: Int): ArrayBuffer[MySymbol] = {
    val buf = new ArrayBuffer[MySymbol]
    data.symbol match {
      case d: IntSymbol =>
        size match {
            //ここから修正開始
          case 8 => buf += (if (d.symbol == 0) setN else clearN)
          case 16 => buf += (if (d.symbol == 0) setN else clearN)
          case 32 => buf += (if (d.symbol == 0) setN else clearN)
        }
      case d: CtxSymbol =>
        val s = ctx.mkSolver
        s.push
        val eq = d >= 0
        s.assertCnstr(eq.symbol)
        val b1 = s.check.get
        s.pop(1)
        s.assertCnstr(ctx.mkNot(eq.symbol))
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
