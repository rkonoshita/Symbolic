package main

import data.DataSet
import symbol.IntSymbol

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/12/11.
 */
class Interrupt {

  def interrupt(d: DataSet): ArrayBuffer[DataSet] = {
    timerB(d.clone())
  }

  def timerB(d: DataSet): ArrayBuffer[DataSet] = {
    execute(0x3A, d)
  }

  def execute(p: Int, d: DataSet): ArrayBuffer[DataSet] = {
    val ccr = d.ccr.ccr
    val pc = d.pc.pc
    val sp = d.reg.getLong(7).asInstanceOf[IntSymbol] - 4
    d.mem.setByte(ccr, sp.symbol)
    d.mem.setWord(new IntSymbol(pc), (sp + 2).symbol)
    d.reg.setLong(sp, 7)
    d.pc.setPc(Main.rom.getWord(p))
    d.ccr.setI
    new Decoder().analyze(d)
  }
}
