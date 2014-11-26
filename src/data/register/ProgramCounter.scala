package data.register

import data.DataSet
import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.{Z3Context, Z3AST}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by ryosuke on 14/11/18.
 */
class ProgramCounter(c: Z3Context, p: Int) {

  var pc = p
  private val ctx = c
  private val limit = 0x0000FFFF

  def setPc(p: Int) = pc = p


}
