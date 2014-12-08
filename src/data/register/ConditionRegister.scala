package data.register

import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by ryosuke on 14/11/18.
 */
class ConditionRegister(ct: Z3Context, c: MySymbol) {

  var ccr = c
  private val ctx = ct

  def clearC = ccr = ccr & 0xFE

  def clearV = ccr = ccr & 0xFD

  def clearZ = ccr = ccr & 0xFB

  def clearN = ccr = ccr & 0xF7

  def clearH = ccr = ccr & 0xDF

  def setC = ccr = ccr | 0x01

  def setV = ccr = ccr | 0x02

  def setZ = ccr = ccr | 0x04

  def setN = ccr = ccr | 0x08

  def setH = ccr = ccr | 0x20

}
