package data.register

import main.Main
import symbol.CtxSymbol
import z3.scala.Z3Context

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/17.
 */
class Memory(c: Z3Context, m: mutable.HashMap[Int, CtxSymbol]) {

  val mem = m
  private val ctx = c
  private val limit = 0xFFFF // ノーマルモード
  //private val limit = 0xFFFFFF //アドバンスモード

  private def check(num: Int*) = num.foreach(n => if (!mem.contains(n & limit)) mem += (n & limit) -> new CtxSymbol(Main.makeSymbol(8)))

  def getByte(num: CtxSymbol): ArrayBuffer[CtxSymbol] = {
    val ans = new ArrayBuffer[CtxSymbol]
    Main.extract(0 to limit, num).foreach { e => ans += getByte(e)}
    ans
  }

  def getByte(num: Int): CtxSymbol = {
    check(num)
    mem(num & limit)
  }

  def getWord(num: CtxSymbol): ArrayBuffer[CtxSymbol] = {
    val ans = new ArrayBuffer[CtxSymbol]
    Main.extract(0 to limit, num).foreach { e => ans += getWord(e)}
    ans
  }

  def getWord(num: Int): CtxSymbol = {
    check(num, num + 1)
    mem(num & limit) :: mem((num + 1) & limit)
  }

  def getLong(num: CtxSymbol): ArrayBuffer[CtxSymbol] = {
    val ans = new ArrayBuffer[CtxSymbol]
    Main.extract(0 to limit, num).foreach { e => ans += getLong(e)}
    ans
  }

  def getLong(num: Int): CtxSymbol = {
    check(num, num + 1, num + 2, num + 3)
    mem(num & limit) :: mem((num + 1) & limit) :: mem((num + 2) & limit) :: mem((num + 3) & limit)
  }

  def setByte(data: CtxSymbol, num: Int) = mem(num & limit) = data

  def setWord(data: CtxSymbol, num: Int): Unit = {
    mem(num & limit) = data.extract(15, 8)
    mem((num + 1) & limit) = data.extract(7, 0)
  }

  def setLong(data: CtxSymbol, num: Int): Unit = {
    mem(num & limit) = data.extract(31, 24)
    mem((num + 1) & limit) = data.extract(23, 16)
    mem((num + 2) & limit) = data.extract(15, 8)
    mem((num + 3) & limit) = data.extract(7, 0)
  }

}
