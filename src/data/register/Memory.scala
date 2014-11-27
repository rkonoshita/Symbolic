package data.register

import main.Main
import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.Z3Context

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/17.
 */
class Memory(c: Z3Context, m: mutable.HashMap[Int, MySymbol]) {

  val mem = m
  private val ctx = c
  private val limit = 0xFFFF

  private def check(num: Int) = if (!mem.contains(num & limit)) mem += (num & limit) -> new CtxSymbol(Main.makeSymbol)

  def getByte(num: Int): MySymbol = {
    check(num)
    mem(num & limit)
  }

  def getWord(num: Int): MySymbol = {
    check(num)
    check(num + 1)
    mem(num & limit) match {
      case m0: IntSymbol =>
        mem((num + 1) & limit) match {
          case m1: IntSymbol => ((m0 & 0xFF) << 8) | (m1 & 0xFF)
          case m1: CtxSymbol => ((m0 & 0xFF) << 8) | (m1 & 0xFF)
        }
      case m0: CtxSymbol =>
        mem((num + 1) & limit) match {
          case m1: IntSymbol => ((m0 & 0xFF) << 8) | (m1 & 0xFF)
          case m1: CtxSymbol => ((m0 & 0xFF) << 8) | (m1 & 0xFF)
        }
    }
  }

  def setByte(data: MySymbol, num: Int): Unit = mem(num & limit) = data

  def setWord(data: MySymbol, num: Int): Unit = {
    data match {
      case d: IntSymbol =>
        mem(num & limit) = (d >> 8) & 0xFF
        mem((num + 1) & limit) = d & 0xFF
      case d: CtxSymbol =>
        mem(num) = (d >> 8) & 0xFF
        mem((num + 1) & limit) = d & 0xFF
    }
  }

}
