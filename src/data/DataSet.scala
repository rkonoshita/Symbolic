package data

import data.register._
import main.State
import symbol.MySymbol

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/12.
 */
class DataSet(r: Register, m: Memory, p: ProgramCounter, c: ConditionRegister, pt: PathCondition, s: SymbolCounter) {

  val reg = r
  val mem = m
  val pc = p
  val ccr = c
  val path = pt
  val counter = s

  override def clone(): DataSet = {
//    val newreg = new mutable.HashMap[Int, MySymbol]
//    reg.reg.foreach(key => newreg += key._1 -> key._2)
//    val newmem = new mutable.HashMap[Int, MySymbol]
//    mem.mem.foreach(key => newmem += key._1 -> key._2)
//    val newcounter = new mutable.HashMap[String, Int]
//    counter.counter.foreach(key => newcounter += key._1 -> key._2)
    val newcounter = new SymbolCounter(counter.counter)
    new DataSet(
      new Register(reg.reg, newcounter),
      new Memory(mem.mem, newcounter),
      new ProgramCounter(pc.pc),
      new ConditionRegister(ccr.ccr),
      new PathCondition(path.path),
      newcounter
    )
  }

  def this(s: State) = this(s.reg, s.mem, s.pc, s.ccr, s.path, s.counter)

}
