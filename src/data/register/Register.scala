package data.register

import main.Main
import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.Z3Context

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
class Register(c: Z3Context, r: mutable.HashMap[Int, MySymbol]) {

  val reg = r
  private val ctx = c
  private val limit = 0x07

  private def check(num: Int): Unit = if (!reg.contains(num & limit)) reg += (num & limit) -> new CtxSymbol(Main.makeSymbol) //指定レジスタに初期値がなければ作る

  //レジスタ値の候補を全て取得する
  def getByte(num: MySymbol): ArrayBuffer[MySymbol] = {
    val ans = new ArrayBuffer[MySymbol]
    num match {
      case n: IntSymbol => ans += getByte(n.symbol)
      case n: CtxSymbol =>
        Main.extract(0 to 0xF, n.symbol).foreach(e => ans += getByte(e))
        ans
    }
  }

  //Intでアクセスする場合は1通り
  def getByte(num: Int): MySymbol = {
    check(num)
    if ((num & 0x8) == 0x8) getByteLow(num)
    else getByteHigh(num)
  }

  private def getByteHigh(num: Int): MySymbol =
    if (reg(num & limit).isInstanceOf[IntSymbol]) (reg(num & limit).asInstanceOf[IntSymbol] >> 8) & 0xFF
    else (reg(num & limit).asInstanceOf[CtxSymbol] >> 8) & 0xFF

  private def getByteLow(num: Int): MySymbol =
    if (reg(num & limit).isInstanceOf[IntSymbol]) reg(num & limit).asInstanceOf[IntSymbol] & 0xFF
    else reg(num & limit).asInstanceOf[CtxSymbol] & 0xFF

  def getWord(num: MySymbol): ArrayBuffer[MySymbol] = {
    val ans = new ArrayBuffer[MySymbol]
    num match {
      case n: IntSymbol => ans += getWord(n.symbol)
      case n: CtxSymbol =>
        Main.extract(0 to 0xF, n.symbol).foreach(e => ans += getWord(e))
        ans
    }
  }

  //Intでアクセスする場合は1通り
  def getWord(num: Int): MySymbol = {
    check(num)
    if ((num & 0x8) == 0x8) getWordHigh(num)
    else getWordLow(num)
  }

  private def getWordHigh(num: Int): MySymbol =
    if (reg(num & limit).isInstanceOf[IntSymbol]) (reg(num & limit).asInstanceOf[IntSymbol] >> 16) & 0xFFFF
    else (reg(num & limit).asInstanceOf[CtxSymbol] >> 16) & 0xFFFF

  private def getWordLow(num: Int): MySymbol =
    if (reg(num & limit).isInstanceOf[IntSymbol]) reg(num & limit).asInstanceOf[IntSymbol] & 0xFFFF
    else reg(num & limit).asInstanceOf[CtxSymbol] & 0xFFFF

  def getLong(num: Int): MySymbol = {
    check(num)
    reg(num & limit)
  }

  //セットすべき値を返す
  def setByte(data: MySymbol, num: Int): Unit = {
    check(num)
    if ((num & 0x8) == 0x8) setByteLow(data, num)
    else setByteHigh(data, num)
  }

  private def setByteHigh(data: MySymbol, num: Int): Unit =
    reg(num & limit) =
      data match {
        case d: IntSymbol =>
          reg(num & limit) match {
            case r: IntSymbol => (r & 0xFFFF00FF) | ((d & 0xFF) << 8)
            case r: CtxSymbol => (r & 0xFFFF00FF) | ((d & 0xFF) << 8)
          }
        case d: CtxSymbol =>
          reg(num & limit) match {
            case r: IntSymbol => (r & 0xFFFF00FF) | ((d & 0xFF) << 8)
            case r: CtxSymbol => (r & 0xFFFF00FF) | ((d & 0xFF) << 8)
          }
      }

  private def setByteLow(data: MySymbol, num: Int): Unit =
    reg(num & limit) =
      data match {
        case d: IntSymbol =>
          reg(num & limit) match {
            case r: IntSymbol => (r & 0xFFFFFF00) | (d & 0xFF)
            case r: CtxSymbol => (r & 0xFFFFFF00) | (d & 0xFF)
          }
        case d: CtxSymbol =>
          reg(num & limit) match {
            case r: IntSymbol => (r & 0xFFFFFF00) | (d & 0xFF)
            case r: CtxSymbol => (r & 0xFFFFFF00) | (d & 0xFF)
          }
      }

  def setLong(data: MySymbol, num: Int) = reg(num & limit) = data

}
