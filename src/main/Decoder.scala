package main

import data.DataSet
import z3.scala.Z3Context

/**
 * Created by rkonoshita on 14/11/17.
 */
class Decoder(c: Z3Context) {

  private final val ctx = c
  private final val bv32 = ctx.mkBVSort(32)

  def analyze(data: DataSet): Unit = {
    val pclist = data.pc.getPC
    pclist.foreach { p =>
      val d = data.clone
      d.path.set(ctx.mkEq(d.pc.pc, ctx.mkInt(p, bv32)))
      pclist.foreach { q =>
        if (p != q) d.path.set(ctx.mkNot(ctx.mkEq(d.pc.pc, ctx.mkInt(q, bv32))))
      }
    }
  }
}
