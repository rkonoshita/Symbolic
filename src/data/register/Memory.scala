package data.register

import base.Symbolic
import symbol.CtxSymbol

/**
 * Created by rkonoshita on 14/11/17.
 */
class Memory(m: CtxSymbol) {

  var mem = m
  //  private val limit = 0xFFFF // ノーマルモード

  private def trans(num: Int): CtxSymbol = new CtxSymbol(num, 16)

  def getByte(num: Int): CtxSymbol = getByte(trans(num))

  def getByte(num: CtxSymbol): CtxSymbol = Symbolic.simple(mem.select(num.extract(15, 0)))

  def getWord(num: Int): CtxSymbol = getWord(trans(num))

  def getWord(num: CtxSymbol) = {
    val number = num.extract(15, 0)
    Symbolic.simple(mem.select(number) concat mem.select(number + 1))
  }

  def getLong(num: Int): CtxSymbol = getLong(trans(num))

  def getLong(num: CtxSymbol): CtxSymbol = {
    val number = num.extract(15, 0)
    Symbolic.simple(mem.select(number) concat mem.select(number + 1)
      concat mem.select(number + 2) concat mem.select(number + 3))
  }

  def setByte(data: CtxSymbol, num: Int): Unit = setByte(data, trans(num))

  def setByte(data: CtxSymbol, num: CtxSymbol) = mem = Symbolic.simple(mem.store(num.extract(15, 0), data))

  def setByte(data: Int, num: CtxSymbol): Unit = setByte(new CtxSymbol(data, 8), num)

  def setByte(data: Int, num: Int): Unit = setByte(new CtxSymbol(data, 8), trans(num))

  def setWord(data: CtxSymbol, num: Int): Unit = setWord(data, trans(num))

  def setWord(data: CtxSymbol, num: CtxSymbol) = {
    val number = num.extract(15, 0)
    mem = Symbolic.simple(mem.store(number, data.extract(15, 8)).store(number + 1, data.extract(7, 0)))
  }

  def setWord(data: Int, num: CtxSymbol): Unit = setWord(new CtxSymbol(data, 16), num)

  def setWord(data: Int, num: Int): Unit = setWord(new CtxSymbol(data, 16), trans(num))

  def setLong(data: CtxSymbol, num: Int): Unit = setLong(data, trans(num))

  def setLong(data: CtxSymbol, num: CtxSymbol) = {
    val number = num.extract(15, 0)
    mem = Symbolic.simple(mem.store(number, data.extract(31, 24)).store(number + 1, data.extract(23, 16))
      .store(number + 2, data.extract(15, 8)).store(number + 3, data.extract(7, 0)))
  }

  def setLong(data: Int, num: CtxSymbol): Unit = setLong(new CtxSymbol(data, 32), num)

  def setLong(data: Int, num: Int): Unit = setLong(new CtxSymbol(data, 32), trans(num))

  override def toString():String = mem.toString()

}
