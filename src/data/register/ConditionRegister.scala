package data.register

import base.Symbolic
import symbol.CtxSymbol

/**
 * Created by ryosuke on 14/11/18.
 */

//条件レジスタ
class ConditionRegister(c: CtxSymbol, limit: Int) {

  private var ccr = c
  val check = limit

  def getCcr: CtxSymbol = ccr.simpleify()

  def setCcr(c: CtxSymbol) = ccr = c.simpleify()

  def clearC = ccr = (ccr & 0xFE).simpleify()

  def clearV = ccr = (ccr & 0xFD).simpleify()

  def clearZ = ccr = (ccr & 0xFB).simpleify()

  def clearN = ccr = (ccr & 0xF7).simpleify()

  def clearH = ccr = (ccr & 0xDF).simpleify()

  def clearI = ccr = (ccr & 0x7F).simpleify()

  def setC = ccr = (ccr | 0x01).simpleify()

  def setV = ccr = (ccr | 0x02).simpleify()

  def setZ = ccr = (ccr | 0x04).simpleify()

  def setN = ccr = (ccr | 0x08).simpleify()

  def setH = ccr = (ccr | 0x20).simpleify()

  def setI = ccr = (ccr | 0x80).simpleify()

  override def toString(): String = ccr.toString()
}
