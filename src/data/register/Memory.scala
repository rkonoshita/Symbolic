package data.register

import data.SymbolCounter
import main.Main
import symbol.CtxSymbol

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/17.
 */
class Memory(m: mutable.HashMap[Int, CtxSymbol], s: SymbolCounter) {

  val mem = m
  val counter = s
  private val ctx = Main.ctx
  private val limit = 0xFFFF // ノーマルモード

  private def check(num: Int*) = num.foreach(n => if (!mem.contains(n & limit)) mem += (n & limit) -> counter.makeSymbol(n & limit, "m"))

  def getByte(num: Int): CtxSymbol = {
    check(num)
    Main.simple(mem(num & limit))
  }

  def getWord(num: Int): CtxSymbol = {
    check(num, num + 1)
    Main.simple(mem(num) concat mem(num + 1))
  }

  def getLong(num: Int): CtxSymbol = {
    check(num, num + 1, num + 2, num + 3)
    Main.simple(mem(num) concat mem(num + 1) concat mem(num + 2) concat mem(num + 3))
  }

  def setByte(data: CtxSymbol, num: Int) = mem(num & limit) = Main.simple(data)

  def setWord(data: CtxSymbol, num: Int): Unit = {
    mem(num & limit) = Main.simple(data.extract(15, 8))
    mem((num + 1) & limit) = Main.simple(data.extract(7, 0))
  }

  def setLong(data: CtxSymbol, num: Int): Unit = {
    mem(num & limit) = Main.simple(data.extract(31, 24))
    mem((num + 1) & limit) = Main.simple(data.extract(23, 16))
    mem((num + 2) & limit) = Main.simple(data.extract(15, 8))
    mem((num + 3) & limit) = Main.simple(data.extract(7, 0))
  }

}
