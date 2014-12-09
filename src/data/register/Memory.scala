package data.register

import main.Main
import symbol.{IntSymbol, MySymbol, CtxSymbol}
import z3.scala.Z3Context

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/17.
 */
class Memory(c: Z3Context, m: mutable.HashMap[Int, MySymbol]) {

  val mem = m
  private val ctx = c
  private val limit = 0xFFFF // ノーマルモード
  //private val limit = 0xFFFFFF //アドバンスモード

  private def check(num: Int*) = num.foreach(n => if (!mem.contains(n & limit)) mem += (n & limit) -> new CtxSymbol(Main.makeSymbol(8)))

  //  def getByte(num: MySymbol): ArrayBuffer[MySymbol] = {
  //    val ans = new ArrayBuffer[MySymbol]
  //    num match {
  //      case n: IntSymbol => ans += getByte(n.symbol)
  //      case n: CtxSymbol => Main.extract(0 to limit, n).foreach { e => ans += getByte(e)}
  //    }
  //    ans
  //  }

  def getByte(num: Int): MySymbol = {
    check(num)
    mem(num & limit)
  }

  //  def getWord(num: MySymbol): ArrayBuffer[MySymbol] = {
  //    val ans = new ArrayBuffer[MySymbol]
  //    num match {
  //      case n: IntSymbol => ans += getWord(n.symbol)
  //      case n: CtxSymbol => Main.extract(0 to limit, n).foreach { e => ans += getWord(e)}
  //    }
  //    ans
  //  }

  def getWord(num: Int): MySymbol = {
    check(num, num + 1)
    (mem(num & limit), mem((num + 1) & limit)) match {
      case (m1: IntSymbol, m2: IntSymbol) => ((m1 & 0xFF) << 8) | (m2 & 0xFF)
      case (m1: IntSymbol, m2: CtxSymbol) => new CtxSymbol(ctx, m1.symbol, 8) concat m2
      case (m1: CtxSymbol, m2: IntSymbol) => m1 concat new CtxSymbol(ctx, m2.symbol, 8)
      case (m1: CtxSymbol, m2: CtxSymbol) => m1 concat m2
    }
  }

  //  def getLong(num: MySymbol): ArrayBuffer[MySymbol] = {
  //    val ans = new ArrayBuffer[MySymbol]
  //    num match {
  //      case n: IntSymbol => ans += getLong(n.symbol)
  //      case n: CtxSymbol => Main.extract(0 to limit, n).foreach { e => ans += getLong(e)}
  //    }
  //    ans
  //  }

  def getLong(num: Int): MySymbol = {
    check(num, num + 1, num + 2, num + 3)
    (mem(num & limit), mem((num + 1) & limit), mem((num + 2) & limit), mem((num + 3) & limit)) match {
      case (m1: IntSymbol, m2: IntSymbol, m3: IntSymbol, m4: IntSymbol) => ((m1 & 0xFF) << 24) | ((m2 & 0xFF) << 16) | ((m3 & 0xFF) << 8) | (m4 & 0xFF)
      case (m1: IntSymbol, m2: IntSymbol, m3: IntSymbol, m4: CtxSymbol) => new CtxSymbol(ctx, (((m1 & 0xFF) << 16) | ((m2 & 0xFF) << 8) | (m3 & 0xFF)).asInstanceOf[IntSymbol].symbol, 24) concat m4
      case (m1: IntSymbol, m2: IntSymbol, m3: CtxSymbol, m4: IntSymbol) => new CtxSymbol(ctx, (((m1 & 0xFF) << 8) | (m2 & 0xFF)).asInstanceOf[IntSymbol].symbol, 16) concat m3 concat new CtxSymbol(ctx, m4.symbol, 8)
      case (m1: IntSymbol, m2: IntSymbol, m3: CtxSymbol, m4: CtxSymbol) => new CtxSymbol(ctx, (((m1 & 0xFF) << 8) | (m2 & 0xFF)).asInstanceOf[IntSymbol].symbol, 16) concat m3 concat m4
      case (m1: IntSymbol, m2: CtxSymbol, m3: IntSymbol, m4: IntSymbol) => new CtxSymbol(ctx, m1.symbol, 8) concat m2 concat new CtxSymbol(ctx, (((m3 & 0xFF) << 8) | (m4 & 0xFF)).asInstanceOf[IntSymbol].symbol, 16)
      case (m1: IntSymbol, m2: CtxSymbol, m3: IntSymbol, m4: CtxSymbol) => new CtxSymbol(ctx, m1.symbol, 8) concat m2 concat new CtxSymbol(ctx, m3.symbol, 8) concat m4
      case (m1: IntSymbol, m2: CtxSymbol, m3: CtxSymbol, m4: IntSymbol) => new CtxSymbol(ctx, m1.symbol, 8) concat m2 concat m3 concat new CtxSymbol(ctx, m4.symbol, 8)
      case (m1: IntSymbol, m2: CtxSymbol, m3: CtxSymbol, m4: CtxSymbol) => new CtxSymbol(ctx, m1.symbol, 8) concat m2 concat m3 concat m4
      case (m1: CtxSymbol, m2: IntSymbol, m3: IntSymbol, m4: IntSymbol) => m1 concat new CtxSymbol(ctx, (((m2 & 0xFF) << 16) | ((m3 & 0xFF) << 8) | (m4 & 0xFF)).asInstanceOf[IntSymbol].symbol, 24)
      case (m1: CtxSymbol, m2: IntSymbol, m3: IntSymbol, m4: CtxSymbol) => m1 concat new CtxSymbol(ctx, (((m2 & 0xFF) << 8) | (m3 & 0xFF)).asInstanceOf[IntSymbol].symbol, 16) concat m4
      case (m1: CtxSymbol, m2: IntSymbol, m3: CtxSymbol, m4: IntSymbol) => m1 concat new CtxSymbol(ctx, m2.symbol, 8) concat m3 concat new CtxSymbol(ctx, m4.symbol, 8)
      case (m1: CtxSymbol, m2: IntSymbol, m3: CtxSymbol, m4: CtxSymbol) => m1 concat new CtxSymbol(ctx, m2.symbol, 8) concat m3 concat m4
      case (m1: CtxSymbol, m2: CtxSymbol, m3: IntSymbol, m4: IntSymbol) => m1 concat m2 concat new CtxSymbol(ctx, (((m3 & 0xFF) << 8) | (m4 & 0xFF)).asInstanceOf[IntSymbol].symbol, 16)
      case (m1: CtxSymbol, m2: CtxSymbol, m3: IntSymbol, m4: CtxSymbol) => m1 concat m2 concat new CtxSymbol(ctx, m3.symbol, 8) concat m4
      case (m1: CtxSymbol, m2: CtxSymbol, m3: CtxSymbol, m4: IntSymbol) => m1 concat m2 concat m3 concat new CtxSymbol(ctx, m4.symbol, 8)
      case (m1: CtxSymbol, m2: CtxSymbol, m3: CtxSymbol, m4: CtxSymbol) => m1 concat m2 concat m3 concat m4
    }
  }

  def setByte(data: MySymbol, num: Int) = mem(num & limit) = data

  def setWord(data: MySymbol, num: Int): Unit = {
    data match {
      case d: IntSymbol =>
        mem(num & limit) = (d >> 8) & 0xFF
        mem((num + 1) & limit) = d & 0xFF
      case d: CtxSymbol =>
        mem(num & limit) = d.extract(15, 8)
        mem((num + 1) & limit) = d.extract(7, 0)
    }

  }

  def setLong(data: MySymbol, num: Int): Unit = {
    data match {
      case d: IntSymbol =>
        mem(num & limit) = (d >> 24) & 0xFF
        mem((num + 1) & limit) = (d >> 16) & 0xFF
        mem((num + 2) & limit) = (d >> 8) & 0xFF
        mem((num + 3) & limit) = d & 0xFF
      case d: CtxSymbol =>
        mem(num & limit) = d.extract(31, 24)
        mem((num + 1) & limit) = d.extract(23, 16)
        mem((num + 2) & limit) = d.extract(15, 8)
        mem((num + 3) & limit) = d.extract(7, 0)
    }

  }

}
