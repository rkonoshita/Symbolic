package data

import data.register._
import main.{Main, State}
import symbol.MySymbol
import z3.scala.{Z3Context, Z3AST}

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
    val newreg = new mutable.HashMap[Int, MySymbol]
    reg.reg.foreach(key => newreg += key._1 -> key._2)
    val newmem = new mutable.HashMap[Int, MySymbol]
    mem.mem.foreach(key => newmem += key._1 -> key._2)
    new DataSet(
      new Register(newreg),
      new Memory(newmem),
      new ProgramCounter(pc.pc),
      new ConditionRegister(ccr.ccr),
      new PathCondition(path.path))
  }

  def this(s: State) = this(s.reg, s.mem, s.pc, s.ccr, s.path)

}
