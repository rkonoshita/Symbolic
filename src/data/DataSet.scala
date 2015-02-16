package data

import data.register._
import base.{Symbolic, State}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */

//寄せ集め
class DataSet(r: Register, m: Memory, p: ProgramCounter, c: ConditionRegister, pt: PathCondition, cs: Array[Boolean]) {

  val reg = new Register(r.reg)
  val mem = new Memory(m.mem, arrayClone(m.in), arrayClone(m.ib.clone))
  val pc = new ProgramCounter(p.pc)
  val ccr = new ConditionRegister(c.getCcr, c.check)
  val path = new PathCondition(pt.path)
  val conset = cs

  override def clone(): DataSet = {
    new DataSet(
      new Register(reg.reg),
      new Memory(mem.mem, mem.in, mem.ib),
      new ProgramCounter(pc.pc),
      new ConditionRegister(ccr.getCcr, ccr.check),
      new PathCondition(path.path),
      conset.clone
    )
  }

  def this(s: State, c: Array[Boolean]) = this(s.reg, s.mem, s.pc, s.ccr, s.path, c)

  def inputCheck(): Unit = {
    if (mem.ib(0)) {
      val input = mem.in(0)
      val pcr3 = mem.getByte(0xFFE6)
      val pdr3 = mem.getByte(0xFFD6)
      val data3 = pdr3 & pcr3 | Symbolic.ctx.mkConst("m" + 0xFFDB + "_" + input, Symbolic.ctx.mkBVSort(8))
      mem.setByte(data3, 0xFFD6)
      mem.in(0) = input + 1
    }
    mem.ib(0) = false
  }

  def arrayClone(array: Array[Int]): Array[Int] = {
    val cp = new Array[Int](array.length)
    for (i <- 0 until array.length) {
      cp(i) = array(i)
    }
    cp
  }

  def arrayClone(array: Array[Boolean]): Array[Boolean] = {
    val cp = new Array[Boolean](array.length)
    for (i <- 0 until array.length) {
      cp(i) = array(i)
    }
    cp
  }

}

//0:stop flag
//1:divop flag