package data.register

import symbol.MySymbol
import z3.scala.Z3Context

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/17.
 */
class Memory(c: Z3Context, m: mutable.HashMap[Int, MySymbol]) {

  val mem = m
  private val ctx = c
  private val mlimit = 0xFFFF

  def getByte(num: Int): MySymbol = mem(num)

}
