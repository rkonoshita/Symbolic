package data.register

import main.Main
import symbol.{CtxSymbol, MySymbol}

/**
 * Created by ryosuke on 14/11/18.
 */
class ConditionRegister(c: MySymbol) {

  var ccr = c

  def clearC = ccr = Main.simple(ccr & 0xFE)

  def clearV = ccr = Main.simple(ccr & 0xFD)

  def clearZ = ccr = Main.simple(ccr & 0xFB)

  def clearN = ccr = Main.simple(ccr & 0xF7)

  def clearH = ccr = Main.simple(ccr & 0xDF)

  def clearI = ccr = Main.simple(ccr & 0x7F)

  def setC = ccr = Main.simple(ccr | 0x01)

  def setV = ccr = Main.simple(ccr | 0x02)

  def setZ = ccr = Main.simple(ccr | 0x04)

  def setN = ccr = Main.simple(ccr | 0x08)

  def setH = ccr = Main.simple(ccr | 0x20)

  def setI = ccr = Main.simple(ccr | 0x80)

}
