package data.register

import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
class Register(c: Z3Context, r: mutable.HashMap[Int, MySymbol]) {

  val reg = r
  private val ctx = c
  private val limit = 0x07

  def getByte(num: MySymbol): ArrayBuffer[MySymbol] = {
    if (num.isInstanceOf[IntSymbol]) getByte(num.asInstanceOf[IntSymbol].symbol)
    else getByte(num.asInstanceOf[CtxSymbol].symbol)
  }

  def getByte(num: Int): ArrayBuffer[MySymbol] =
    if ((num & 0x8) == 0x8) ArrayBuffer[MySymbol](getByteLow(num))
    else ArrayBuffer[MySymbol](getByteHigh(num))

  def getByte(num: Z3AST): ArrayBuffer[MySymbol] = {
    val s = ctx.mkSolver
    val ans = new ArrayBuffer[MySymbol]
    (0 until 0x0F).foreach { n =>
      s.push
      s.assertCnstr(ctx.mkEq(num, ctx.mkInt(n, num.getSort)))
      if (s.check.get) ans ++= getByte(n)
      s.pop(1)
    }
    ans
  }

  private def getByteHigh(num: Int): MySymbol =
    if (reg(num).isInstanceOf[IntSymbol]) (reg(num).asInstanceOf[IntSymbol] >> 8).asInstanceOf[IntSymbol] & 0xFF
    else (reg(num).asInstanceOf[CtxSymbol] >> 8).asInstanceOf[CtxSymbol] & 0xFF

  private def getByteLow(num: Int): MySymbol =
    if (reg(num).isInstanceOf[IntSymbol]) reg(num).asInstanceOf[IntSymbol] & 0xFF
    else reg(num).asInstanceOf[CtxSymbol] & 0xFF


}
