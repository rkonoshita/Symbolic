package data.register

import main.{Main, Parameter}
import symbol.CtxSymbol

/**
 * Created by ryosuke on 14/11/18.
 */

//記号的に扱うべきか悩ましいところ
class ProgramCounter(p: Int) {

  private val limit = 0x0000FFFF
  var pc = p & limit

  def setPc(p: Int) = pc = p & limit

  def setPc(p:CtxSymbol): Unit = {
    (0x0100 to Parameter.size("P")).foreach { q =>
      Main.sol.assertCnstr(p.eq(q).symbol)
      if(Main.sol.check.get) {
        Main.sol.reset
        pc = q
        return
      } else Main.sol.reset
    }
  }

  override def toString(): String = pc.toString

}
