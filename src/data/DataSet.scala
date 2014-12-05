package data

import data.register._
import main.{Main, State}
import symbol.CtxSymbol

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/12.
 */
class DataSet(r: Register, m: Memory, p: ProgramCounter, c: ConditionRegister, pt: PathCondition) {

  val reg = r
  val mem = m
  val pc = p
  val ccr = c
  val path = pt

  override def clone(): DataSet = {
    val newreg = new mutable.HashMap[Int, CtxSymbol]
    reg.reg.foreach(key => newreg += key._1 -> key._2)
    val newmem = new mutable.HashMap[Int, CtxSymbol]
    mem.mem.foreach(key => newmem += key._1 -> key._2)
    new DataSet(
      new Register(Main.ctx, newreg),
      new Memory(Main.ctx, newmem),
      new ProgramCounter(pc.pc),
      new ConditionRegister(Main.ctx, ccr.ccr),
      new PathCondition(Main.ctx, path.path))
  }

  def this(s: State) = this(s.reg, s.mem, s.pc, s.ccr, s.path)

}
