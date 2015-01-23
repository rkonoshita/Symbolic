package main

import data.DataSet

import scala.collection.mutable.ArrayBuffer

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
      Main.sol.assertCnstr(path.path)
      val ans = Main.sol.check.get
      Main.sol.reset
      ans
    }
  val stop = data.pc.pc == (Main.rom.getWord(0) + 14) | !pathCheck

  override def toString(): String =
    if (path.path == null) number + ", " + true
    else number + ", " + path.path.toString()


  //    override def toString(): String = ccr.ccr.toString

}
