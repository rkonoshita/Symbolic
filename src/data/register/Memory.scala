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
    //入出力の利用判定
    num & 0xFFFF match {
      case 0xFFD4 => ib(0) = true
      case 0xFFD5 => ib(1) = true
      case 0xFFD6 => ib(2) = true
      case 0xFFD8 => ib(3) = true
      case 0xFFD9 => ib(4) = true
      case 0xFFDA => ib(5) = true
      case 0xFFDB => ib(6) = true
      case 0xFFDD => ib(7) = true
      case _ =>
    }
    getByte(trans(num))
  }

  //むしろこっちに入出力判定が必要
  //SMT Solver使うしかない？
  def getByte(num: CtxSymbol): CtxSymbol = mem.select(num.extract(15, 0))

  //ワードサイズ以上は奇数アドレスでのアクセス禁止なので
  //そこもやれたらいいと思います
  def getWord(num: Int): CtxSymbol = {
    num & 0xFFFF match {
      case 0xFFD4 =>
        ib(0) = true
        ib(1) = true
      case 0xFFD6 => ib(2) = true
      case 0xFFD8 =>
        ib(3) = true
        ib(4) = true
      case 0xFFDA =>
        ib(5) = true
        ib(6) = true
      case 0xFFDD => ib(7) = true
      case _ =>
    }
    getWord(trans(num))
  }

  def getWord(num: CtxSymbol) = {
    val number = num.extract(15, 0)
    mem.select(number) concat mem.select(number + 1)
  }

  def getLong(num: Int): CtxSymbol = {
    num & 0xFFFF match {
      case 0xFFD4 =>
        ib(0) = true
        ib(1) = true
        ib(2) = true
      case 0xFFD6 =>
        ib(2) = true
        ib(3) = true
        ib(4) = true
      case 0xFFD8 =>
        ib(3) = true
        ib(4) = true
        ib(5) = true
        ib(6) = true
      case 0xFFDA =>
        ib(5) = true
        ib(6) = true
        ib(7) = true
      case 0xFFDD => ib(7) = true
      case _ =>
    }
    getLong(trans(num))
  }

  def getLong(num: CtxSymbol): CtxSymbol = {
    val number = num.extract(15, 0)
    (mem.select(number) concat mem.select(number + 1)
      concat mem.select(number + 2) concat mem.select(number + 3))
  }

  def setByte(data: CtxSymbol, num: Int): Unit = setByte(data, trans(num))

  def setByte(data: CtxSymbol, num: CtxSymbol) = mem = mem.store(num.extract(15, 0), data)

  def setByte(data: Int, num: CtxSymbol): Unit = setByte(new CtxSymbol(data, 8), num)

  def setByte(data: Int, num: Int): Unit = setByte(new CtxSymbol(data, 8), trans(num))

  def setWord(data: CtxSymbol, num: Int): Unit = setWord(data, trans(num))

  def setWord(data: CtxSymbol, num: CtxSymbol) = {
    val number = num.extract(15, 0)
    mem = mem.store(number, data.extract(15, 8)).store(number + 1, data.extract(7, 0))
  }

  def setWord(data: Int, num: CtxSymbol): Unit = setWord(new CtxSymbol(data, 16), num)

  def setWord(data: Int, num: Int): Unit = setWord(new CtxSymbol(data, 16), trans(num))

  def setLong(data: CtxSymbol, num: Int): Unit = setLong(data, trans(num))

  def setLong(data: CtxSymbol, num: CtxSymbol) = {
    val number = num.extract(15, 0)
    mem = mem.store(number, data.extract(31, 24)).store(number + 1, data.extract(23, 16))
      .store(number + 2, data.extract(15, 8)).store(number + 3, data.extract(7, 0))
  }

  def setLong(data: Int, num: CtxSymbol): Unit = setLong(new CtxSymbol(data, 32), num)

  def setLong(data: Int, num: Int): Unit = setLong(new CtxSymbol(data, 32), trans(num))

  override def toString(): String = mem.toString()

}
