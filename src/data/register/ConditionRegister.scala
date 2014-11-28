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

  def clearC: MySymbol = ccr & 0xFE

  def clearV: MySymbol = ccr & 0xFD

  def clearZ: MySymbol = ccr & 0xFB

  def clearN: MySymbol = ccr & 0xF7

  def clearH: MySymbol = ccr & 0xEF

  def setC: MySymbol = ccr | 0x01

  def setV: MySymbol = ccr | 0x02

  def setZ: MySymbol = ccr | 0x04

  def setN: MySymbol = ccr | 0x08

  def setH: MySymbol = ccr | 0x10

}
