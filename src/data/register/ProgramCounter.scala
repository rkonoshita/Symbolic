package data.register

import base.{Symbolic, Parameter}
import symbol.CtxSymbol

/**
 * Created by ryosuke on 14/11/18.
 */

//プログラムカウンタ
//記号的に扱うべきか悩ましいところ
class ProgramCounter(p: Int) {

  private val limit = 0x0000FFFF
  var pc = p & limit

  def setPc(p: Int) = pc = p & limit

  def setPc(p:CtxSymbol): Int = { //むちゃくちゃ
    (0x0100 to 0x0E00).foreach { q =>
      Symbolic.sol.assertCnstr(p.equal(q).symbol)
      if(Symbolic.sol.check.get) {
        Symbolic.sol.reset
        pc = q
        return pc
      } else Symbolic.sol.reset
    }
    return -1
  }

  override def toString(): String = pc.toString

}
