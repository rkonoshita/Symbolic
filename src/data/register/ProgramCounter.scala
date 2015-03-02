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

  //すごい無理やりやってる
  def setPc(p: CtxSymbol): Unit =
    pc = Integer.parseInt(p.simpleify().toString().replace("#x", ""), 16) & limit

  override def toString(): String = pc.toString

}
