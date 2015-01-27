package data.register

import base.Symbolic
import symbol.CtxSymbol

/**
 * Created by rkonoshita on 14/11/12.
 */
class Register(r: CtxSymbol) {

  var reg = r

  private def trans(num: Int): CtxSymbol = new CtxSymbol(num, 4)

  def getByte(num: Int): CtxSymbol =
    Symbolic.simple(if ((num & 0x8) == 0x8) getByteLow(num)
    else getByteHigh(num))

  private def getByteHigh(num: Int): CtxSymbol = reg.select(trans(num) & 0x07).extract(15, 8)

  private def getByteLow(num: Int): CtxSymbol = reg.select(trans(num) & 0x07).extract(7, 0)

  def getWord(num: Int): CtxSymbol =
    Symbolic.simple(if ((num & 0x8) == 0x8) getWordHigh(num)
    else getWordLow(num))

  private def getWordHigh(num: Int): CtxSymbol = reg.select(trans(num) & 0x07).extract(31, 16)

  private def getWordLow(num: Int): CtxSymbol = reg.select(trans(num) & 0x07).extract(15, 0)

  def getLong(num: Int): CtxSymbol = Symbolic.simple(reg.select(trans(num) & 0x07))

  def setByte(data: CtxSymbol, num: Int): Unit =
    if ((num & 0x8) == 0x8) setByteLow(data, num)
    else setByteHigh(data, num)

  private def setByteHigh(data: CtxSymbol, num: Int): Unit = {
    val n = trans(num) & 0x07
    val base = reg.select(n)
    reg = reg.store(n, Symbolic.simple(base.extract(31, 16) concat data concat base.extract(7, 0)))
  }

  private def setByteLow(data: CtxSymbol, num: Int): Unit = {
    val n = trans(num) & 0x07
    reg = reg.store(n, Symbolic.simple(reg.select(n).extract(31, 16) concat data))
  }

  def setWord(data: CtxSymbol, num: Int): Unit =
    if ((num & 0x8) == 0x8) setWordHigh(data, num)
    else setWordLow(data, num)


  private def setWordHigh(data: CtxSymbol, num: Int) = {
    val n = trans(num) & 0x07
    reg = reg.store(n, Symbolic.simple(data concat reg.select(n).extract(15, 0)))
  }

  private def setWordLow(data: CtxSymbol, num: Int) = {
    val n = trans(num) & 0x07
    reg = reg.store(n, Symbolic.simple(reg.select(n).extract(31, 16) concat data))
  }

  def setLong(data: CtxSymbol, num: Int): Unit = reg = Symbolic.simple(reg.store(trans(num) & 0x07, data))

}
