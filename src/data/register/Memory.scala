package data.register

import symbol.CtxSymbol

/**
 * Created by rkonoshita on 14/11/17.
 */

//メモリ
//データサイズ8bit
//記号地によるメモリアクセスは16bitで可能
class Memory(m: CtxSymbol, inNum: Array[Int], inBool: Array[Boolean]) {

  var mem = m
  var in = inNum
  var ib = inBool
  //  private val limit = 0xFFFF // ノーマルモード

  private def trans(num: Int): CtxSymbol = new CtxSymbol(num, 16)

  def getByte(num: Int): CtxSymbol = {
    if ((num & 0x0000FFFF) == 0x0000FFD6) //入力があればtrueにする。ダサイ
      ib(2) = true
    getByte(trans(num))
  }

  def getByte(num: CtxSymbol): CtxSymbol = mem.select(num.extract(15, 0)).simpleify()

  def getWord(num: Int): CtxSymbol = {
    getWord(trans(num))
  }

  def getWord(num: CtxSymbol) = {
    val number = num.extract(15, 0)
    mem.select(number) concat mem.select(number + 1).simpleify()
  }

  def getLong(num: Int): CtxSymbol = getLong(trans(num))

  def getLong(num: CtxSymbol): CtxSymbol = {
    val number = num.extract(15, 0)
    (mem.select(number) concat mem.select(number + 1)
      concat mem.select(number + 2) concat mem.select(number + 3)).simpleify()
  }

  def setByte(data: CtxSymbol, num: Int): Unit = setByte(data, trans(num))

  def setByte(data: CtxSymbol, num: CtxSymbol) = mem = mem.store(num.extract(15, 0), data).simpleify()

  def setByte(data: Int, num: CtxSymbol): Unit = setByte(new CtxSymbol(data, 8), num)

  def setByte(data: Int, num: Int): Unit = setByte(new CtxSymbol(data, 8), trans(num))

  def setWord(data: CtxSymbol, num: Int): Unit = setWord(data, trans(num))

  def setWord(data: CtxSymbol, num: CtxSymbol) = {
    val number = num.extract(15, 0)
    mem = mem.store(number, data.extract(15, 8)).store(number + 1, data.extract(7, 0)).simpleify()
  }

  def setWord(data: Int, num: CtxSymbol): Unit = setWord(new CtxSymbol(data, 16), num)

  def setWord(data: Int, num: Int): Unit = setWord(new CtxSymbol(data, 16), trans(num))

  def setLong(data: CtxSymbol, num: Int): Unit = setLong(data, trans(num))

  def setLong(data: CtxSymbol, num: CtxSymbol) = {
    val number = num.extract(15, 0)
    mem = mem.store(number, data.extract(31, 24)).store(number + 1, data.extract(23, 16))
      .store(number + 2, data.extract(15, 8)).store(number + 3, data.extract(7, 0)).simpleify()
  }

  def setLong(data: Int, num: CtxSymbol): Unit = setLong(new CtxSymbol(data, 32), num)

  def setLong(data: Int, num: Int): Unit = setLong(new CtxSymbol(data, 32), trans(num))

  def checkIOPortAddress(): Unit = {

  }

  override def toString(): String = mem.toString()

}
