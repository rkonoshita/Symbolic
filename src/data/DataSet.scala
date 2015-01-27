package data

import data.register._
import base.State

/**
 * Created by rkonoshita on 14/11/12.
 */
class DataSet(r: Register, m: Memory, p: ProgramCounter, c: ConditionRegister, pt: PathCondition) {

  val reg = r
  val mem = m
  val pc = p
  val ccr = c
  val path = pt
  var stop = false

  override def clone(): DataSet = {
    new DataSet(
      new Register(reg.reg),
      new Memory(mem.mem),
      new ProgramCounter(pc.pc),
      new ConditionRegister(ccr.getCcr),
      new PathCondition(path.path)
    )
  }

  def this(s: State) = this(s.reg, s.mem, s.pc, s.ccr, s.path)

}
