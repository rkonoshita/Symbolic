package main

import data.DataSet
import symbol.IntSymbol

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/12/01.
 */
class State(data: DataSet, pr: State) {

  val pre = if (pr == null) null else ArrayBuffer[State](pr)
  val next = new ArrayBuffer[State]
  val reg = data.reg
  val mem = data.mem
  val pc = data.pc
  val ccr = data.ccr
  val path = data.path
  val stop = pc.pc == (mem.getWord(0) + 14).asInstanceOf[IntSymbol].symbol

  override def toString(): String = if (path.path == null) "null" else path.path.toString()
}
