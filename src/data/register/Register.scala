package data.register

import main.Main
import symbol.{IntSymbol, CtxSymbol, MySymbol}
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

  private def check(num: Int): Unit = if (!reg.contains(num & limit)) reg += (num & limit) -> new CtxSymbol(Main.makeRegisterSymbol) //指定レジスタに初期値がなければ作る

  //レジスタ値の候補を全て取得する
  //  def getByte(num: MySymbol): ArrayBuffer[MySymbol] = {
  //    val ans = new ArrayBuffer[MySymbol]
  //    num match {
  //      case n: IntSymbol => ans += getByte(n.symbol)
  //      case n: CtxSymbol => Main.extract(0 to 0xF, n).foreach(e => ans += getByte(e))
  //    }
  //    ans
  //  }

  //Intでアクセスする場合は1通り
  def getByte(num: Int): MySymbol = {
    check(num)
    if ((num & 0x8) == 0x8) getByteLow(num)
    else getByteHigh(num)
  }

  private def getByteHigh(num: Int): MySymbol =
    reg(num & limit) match {
      case r: IntSymbol => (r & 0x0000FF00) >> 8
      case r: CtxSymbol => r.extract(15, 8)
    }

  private def getByteLow(num: Int): MySymbol =
    reg(num & limit) match {
      case r: IntSymbol => (r >> 8) & 0x000000FF
      case r: CtxSymbol => r.extract(7, 0)
    }

  //  def getWord(num: MySymbol): ArrayBuffer[MySymbol] = {
  //    val ans = new ArrayBuffer[MySymbol]
  //    num match {
  //      case n: IntSymbol => ans += getWord(n.symbol)
  //      case n: CtxSymbol => Main.extract(0 to 0xF, n).foreach(e => ans += getWord(e))
  //    }
  //    ans
  //  }

  def getWord(num: Int): MySymbol = {
    check(num)
    if ((num & 0x8) == 0x8) getWordHigh(num)
    else getWordLow(num)
  }

  private def getWordHigh(num: Int): MySymbol =
    reg(num & limit) match {
      case r: IntSymbol => (r >> 16) & 0x0000FFFF
      case r: CtxSymbol => r.extract(31, 16)
    }

  private def getWordLow(num: Int): MySymbol =
    reg(num & limit) match {
      case r: IntSymbol => r & 0x0000FFFF
      case r: CtxSymbol => r.extract(15, 0)
    }

  //  def getLong(num: MySymbol): ArrayBuffer[MySymbol] = {
  //    val ans = new ArrayBuffer[MySymbol]
  //    num match {
  //      case n: IntSymbol => ans += getLong(n.symbol)
  //      case n: CtxSymbol => Main.extract(0 to 0xF, n).foreach(e => ans += getLong(e))
  //    }
  //    ans
  //  }

  def getLong(num: Int): MySymbol = {
    check(num)
    reg(num & limit)
  }

  def setByte(data: MySymbol, num: Int): Unit = {
    check(num)
    if ((num & 0x8) == 0x8) setByteLow(data, num)
    else setByteHigh(data, num)
  }

  private def setByteHigh(data: MySymbol, num: Int) =
    reg(num & limit) =
      (data, reg(num & limit)) match {
        case (d: IntSymbol, r: IntSymbol) => (r & 0xFFFF00FF) | ((d & 0x000000FF) << 8)
        case (d: IntSymbol, r: CtxSymbol) => r.extract(31, 16).concat(new CtxSymbol(ctx, d.symbol, 8)).concat(r.extract(7, 0))
        case (d: CtxSymbol, r: IntSymbol) =>
          val rn = new CtxSymbol(ctx, r.symbol, 32)
          rn.extract(31, 16).concat(d).concat(rn.extract(7, 0))
        case (d: CtxSymbol, r: CtxSymbol) => r.extract(31, 16).concat(d).concat(r.extract(7, 0))
      }


  private def setByteLow(data: MySymbol, num: Int) =
    reg(num & limit) =
      (data, reg(num & limit)) match {
        case (d: IntSymbol, r: IntSymbol) => (r & 0xFFFF00FF) | (d & 0x000000FF)
        case (d: IntSymbol, r: CtxSymbol) => r.extract(31, 8).concat(new CtxSymbol(ctx, d.symbol, 8))
        case (d: CtxSymbol, r: IntSymbol) => new CtxSymbol(ctx, r.symbol, 32).extract(31, 8).concat(d)
        case (d: CtxSymbol, r: CtxSymbol) => r.extract(31, 8).concat(d)
      }

  def setWord(data: MySymbol, num: Int): Unit = {
    check(num)
    if ((num & 0x8) == 0x8) setWordHigh(data, num)
    else setWordLow(data, num)
  }

  private def setWordHigh(data: MySymbol, num: Int) =
    reg(num & limit) =
      (data, reg(num & limit)) match {
        case (d: IntSymbol, r: IntSymbol) => ((d & 0x0000FFFF) << 16) | (r & 0x0000FFFF)
        case (d: IntSymbol, r: CtxSymbol) => new CtxSymbol(ctx, d.symbol, 16).concat(r.extract(15, 0))
        case (d: CtxSymbol, r: IntSymbol) => d.concat(new CtxSymbol(ctx, r.symbol, 32).extract(15, 0))
        case (d: CtxSymbol, r: CtxSymbol) => d.concat(r.extract(15, 0))
      }

  private def setWordLow(data: MySymbol, num: Int) =
    reg(num & limit) =
      (data, reg(num & limit)) match {
        case (d: IntSymbol, r: IntSymbol) => ((r & 0xFFFF0000) << 16) | (d & 0x0000FFFF)
        case (d: IntSymbol, r: CtxSymbol) => r.extract(31, 16).concat(new CtxSymbol(ctx, d.symbol, 16))
        case (d: CtxSymbol, r: IntSymbol) => new CtxSymbol(ctx, r.symbol, 32).extract(31, 16).concat(d)
        case (d: CtxSymbol, r: CtxSymbol) => r.extract(31, 16).concat(d)
      }

  def setLong(data: MySymbol, num: Int) = reg(num & limit) = data

}
