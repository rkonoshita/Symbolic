package main

import data.DataSet
import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.Z3AST

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, HashMap}

/**
 * Created by rkonoshita on 14/12/01.
 */
class State(num: Int, data: DataSet, pr: State) {

  val number = num
  val pre = if (pr == null) null else ArrayBuffer[State](pr)
  val next = new ArrayBuffer[State]
  val reg = data.reg
  val mem = data.mem
  val pc = data.pc
  val ccr = data.ccr
  val path = data.path
  val counter = data.counter
  val pathCheck =
    if (path.path == null) true
    else {
      val s = Main.ctx.mkSolver
      s.assertCnstr(path.path)
      s.check.get
    }
  val stop = data.pc.pc == (Main.rom.getWord(0) + 14) | !pathCheck

  override def toString(): String =
    if (path.path == null) number + ", " + true
    else number + ", " + path.path.toString()


  //    override def toString(): String = ccr.ccr.toString

}
