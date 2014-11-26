package main

import data.DataSet
import symbol.{CtxSymbol, IntSymbol}
import z3.scala.Z3Context

/**
 * Created by rkonoshita on 14/11/17.
 */
class Decoder(c: Z3Context) {

  private final val ctx = c

  def analyze(data: DataSet): Unit = {
    decode(data.clone, data.pc.pc)
  }

  def decode(data: DataSet, pc: Int) = {
    data.mem.getByte(pc) match {
      case op0: IntSymbol => //後々のために…
        op0.symbol & 0xF0 match {
          case 0xF0 =>
            //MOV.B Imm,Reg [Freg][Imm]
            val imm = data.mem.getByte(pc + 1)
            data.ccr.ccr = data.ccr.clear("V")
            data.ccr.checkZ(imm)
            data.ccr.checkN(imm)
        }
    }
  }
}
