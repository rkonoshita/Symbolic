package data.register

import symbol.MySymbol

/**
 * Created by ryosuke on 14/11/18.
 */
class ConditionRegister(c: MySymbol) {

  var ccr = c

  def clearC = ccr = ccr & 0xFE

  def clearV = ccr = ccr & 0xFD

  def clearZ = ccr = ccr & 0xFB

  def clearN = ccr = ccr & 0xF7

  def clearH = ccr = ccr & 0xDF

  def clearI = ccr = ccr & 0x7F

  def setC = ccr = ccr | 0x01

  def setV = ccr = ccr | 0x02

  def setZ = ccr = ccr | 0x04

  def setN = ccr = ccr | 0x08

  def setH = ccr = ccr | 0x20

  def setI = ccr = ccr | 0x80

}
