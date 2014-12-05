package data.register

import main.Main
import symbol.CtxSymbol
import z3.scala.Z3Context

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
class Register(c: Z3Context, r: mutable.HashMap[Int, CtxSymbol]) {

  val reg = r
  private val ctx = c
  private val limit = 0x07

  private def check(num: Int): Unit = if (!reg.contains(num & limit)) reg += (num & limit) -> new CtxSymbol(Main.makeSymbol(32)) //指定レジスタに初期値がなければ作る

  //レジスタ値の候補を全て取得する
  def getByte(num: CtxSymbol): ArrayBuffer[CtxSymbol] = {
    val ans = new ArrayBuffer[CtxSymbol]
    Main.extract(0 to 0xF, num).foreach(e => ans += getByte(e))
    ans
  }

  //Intでアクセスする場合は1通り
  def getByte(num: Int): CtxSymbol = {
    check(num)
    if ((num & 0x8) == 0x8) getByteLow(num)
    else getByteHigh(num)
  }

  private def getByteHigh(num: Int): CtxSymbol = reg(num & limit).extract(15, 8)

  private def getByteLow(num: Int): CtxSymbol = reg(num & limit).extract(7, 0)

  def getWord(num: CtxSymbol): ArrayBuffer[CtxSymbol] = {
    val ans = new ArrayBuffer[CtxSymbol]
    Main.extract(0 to 0xF, num).foreach(e => ans += getWord(e))
    ans
  }

  def getWord(num: Int): CtxSymbol = {
    check(num)
    if ((num & 0x8) == 0x8) getWordHigh(num)
    else getWordLow(num)
  }

  private def getWordHigh(num: Int): CtxSymbol = reg(num & limit).extract(31, 16)

  private def getWordLow(num: Int): CtxSymbol = reg(num & limit).extract(15, 0)

  def getLong(num: CtxSymbol): ArrayBuffer[CtxSymbol] = {
    val ans = new ArrayBuffer[CtxSymbol]
    Main.extract(0 to 0xF, num).foreach(e => ans += getLong(e))
    ans
  }

  def getLong(num: Int): CtxSymbol = {
    check(num)
    reg(num & limit)
  }

  def setByte(data: CtxSymbol, num: Int): Unit = {
    check(num)
    if ((num & 0x8) == 0x8) setByteLow(data, num)
    else setByteHigh(data, num)
  }

  private def setByteHigh(data: CtxSymbol, num: Int) =
    reg(num & limit) = reg(num & limit).extract(31, 16) :: data :: reg(num & limit).extract(7, 0)

  private def setByteLow(data: CtxSymbol, num: Int) =
    reg(num & limit) = reg(num & limit).extract(31, 8) :: data

  def setWord(data: CtxSymbol, num: Int): Unit = {
    check(num)
    if ((num & 0x8) == 0x8) setWordHigh(data, num)
    else setWordLow(data, num)
  }

  private def setWordHigh(data: CtxSymbol, num: Int) =
    reg(num & limit) = data :: reg(num & limit).extract(15, 0)

  private def setWordLow(data: CtxSymbol, num: Int) =
    reg(num & limit) = reg(num & limit).extract(31, 16) :: data

  def setLong(data: CtxSymbol, num: Int) = reg(num & limit) = data

}
