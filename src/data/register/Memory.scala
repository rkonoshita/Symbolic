package data.register

import main.Main
import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.Z3Context

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/17.
 */
class Memory(c: Z3Context, m: mutable.HashMap[Int, MySymbol]) {

  val mem = m
  private val ctx = c
  private val limit = 0xFFFF

  private def check(num: Int*) = num.foreach(n => if (!mem.contains(n & limit)) mem += (n & limit) -> new CtxSymbol(Main.makeSymbol))

  def getByte(num: MySymbol): ArrayBuffer[MySymbol] = {
    val ans = new ArrayBuffer[MySymbol]
    num match {
      case n: IntSymbol => ans += getByte(n.symbol)
      case n: CtxSymbol => Main.extract(0 to limit, n.symbol).foreach { e => ans += getByte(e)}
    }
    ans
  }

  def getByte(num: Int): MySymbol = {
    check(num)
    mem(num & limit)
  }

  def getWord(num: Int): MySymbol = {
    check(num, num + 1)
    (((mem(num & limit)) & 0xFF) << 8) | (mem((num + 1) & limit) & 0xFF)
  }

  def getLong(num: Int): MySymbol = {
    check(num, num + 1, num + 2, num + 3)
    ((mem(num & limit) & 0xFF) << 24) | ((mem((num + 1) & limit) & 0xFF) << 16) | ((mem((num + 2) & limit) & 0xFF) << 8) | (mem((num + 3) & limit) & 0xFF)
  }

  def setByte(data: MySymbol, num: Int): Unit = mem(num & limit) = data

  def setWord(data: MySymbol, num: Int): Unit = {
    mem(num & limit) = (data >> 8) & 0xFF
    mem((num + 1) & limit) = data & 0xFF
  }

  def setLong(data: MySymbol, num: Int): Unit = {
    mem(num & limit) = (data >> 24) & 0xFF
    mem((num + 1) & limit) = (data >> 16) & 0xFF
    mem((num + 2) & limit) = (data >> 8) & 0xFF
    mem((num + 3) & limit) = data & 0xFF
  }

}
