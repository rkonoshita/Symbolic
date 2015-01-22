package data.register

import data.SymbolCounter
import main.Main
import symbol.CtxSymbol

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/12.
 */
class Register(r: mutable.HashMap[Int, CtxSymbol], s: SymbolCounter) {

  val reg = r
  val counter = s
  private val ctx = Main.ctx
  private val limit = 0x07

  private def check(num: Int): Unit = if (!reg.contains(num & limit)) reg += (num & limit) -> counter.makeSymbol(num & limit, "r") //指定レジスタに初期値がなければ作る

  def getByte(num: Int): CtxSymbol = {
    check(num)
    Main.simple(if ((num & 0x8) == 0x8) getByteLow(num)
    else getByteHigh(num))
  }

  private def getByteHigh(num: Int): CtxSymbol = reg(num & limit).extract(15, 8)

  private def getByteLow(num: Int): CtxSymbol = reg(num & limit).extract(7, 0)

  def getWord(num: Int): CtxSymbol = {
    check(num)
    Main.simple(if ((num & 0x8) == 0x8) getWordHigh(num)
    else getWordLow(num))
  }

  private def getWordHigh(num: Int): CtxSymbol = reg(num & limit).extract(31, 16)

  private def getWordLow(num: Int): CtxSymbol = reg(num & limit).extract(15, 0)

  def getLong(num: Int): CtxSymbol = {
    check(num)
    Main.simple(reg(num & limit))
  }

  def setByte(data: CtxSymbol, num: Int): Unit = {
    check(num)
    if ((num & 0x8) == 0x8) setByteLow(data, num)
    else setByteHigh(data, num)
  }

  private def setByteHigh(data: CtxSymbol, num: Int) =
    reg(num & limit) = Main.simple(reg(num & limit).extract(31, 16) concat data concat reg(num & limit).extract(7, 0))


  private def setByteLow(data: CtxSymbol, num: Int) = reg(num & limit) = Main.simple(reg(num & limit).extract(31, 8) concat data)

  def setWord(data: CtxSymbol, num: Int): Unit = {
    check(num)
    if ((num & 0x8) == 0x8) setWordHigh(data, num)
    else setWordLow(data, num)
  }

  private def setWordHigh(data: CtxSymbol, num: Int) = reg(num & limit) = Main.simple(data concat reg(num & limit).extract(15, 0))

  private def setWordLow(data: CtxSymbol, num: Int) = reg(num & limit) = Main.simple(reg(num & limit).extract(31, 16) concat data)

  def setLong(data: CtxSymbol, num: Int) = reg(num & limit) = Main.simple(data)

}
