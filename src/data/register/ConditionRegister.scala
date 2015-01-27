package data.register

import base.Symbolic
import symbol.CtxSymbol

/**
 * Created by ryosuke on 14/11/18.
 */
class ConditionRegister(c: CtxSymbol) {

  private var ccr = c

  def getCcr: CtxSymbol = Symbolic.simple(ccr)

  def setCcr(c: CtxSymbol) = ccr = Symbolic.simple(c)

  def clearC = ccr = Symbolic.simple(ccr & 0xFE)

  def clearV = ccr = Symbolic.simple(ccr & 0xFD)

  def clearZ = ccr = Symbolic.simple(ccr & 0xFB)

  def clearN = ccr = Symbolic.simple(ccr & 0xF7)

  def clearH = ccr = Symbolic.simple(ccr & 0xDF)

  def clearI = ccr = Symbolic.simple(ccr & 0x7F)

  def setC = ccr = Symbolic.simple(ccr | 0x01)

  def setV = ccr = Symbolic.simple(ccr | 0x02)

  def setZ = ccr = Symbolic.simple(ccr | 0x04)

  def setN = ccr = Symbolic.simple(ccr | 0x08)

  def setH = ccr = Symbolic.simple(ccr | 0x20)

  def setI = ccr = Symbolic.simple(ccr | 0x80)

  override def toString(): String = ccr.toString()
}
