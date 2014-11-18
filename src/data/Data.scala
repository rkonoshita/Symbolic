package data

import data.register._
import z3.scala.{Z3Context, Z3AST}

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/12.
 */
class Data(z3: Z3Context, r: Register, m: Memory, p: ProgramCounter, c: ConditionRegister, pt: PathCondition) {

  val ctx = z3
  val reg = r
  val mem = m
  val pc = p
  val ccr = c
  val path = pt

  override def clone(): Data = {
    val newreg = new mutable.HashMap[Int, Z3AST]
    reg.reg.foreach(key => newreg += key._1 -> key._2)
    val newmem = new mutable.HashMap[Int, Z3AST]
    mem.mem.foreach(key => newmem += key._1 -> key._2)
    new Data(ctx, new Register(ctx, newreg), new Memory(ctx, newmem), new ProgramCounter(ctx, pc.pc), new ConditionRegister(ccr.ccr), new PathCondition(path.path))
  }

}
