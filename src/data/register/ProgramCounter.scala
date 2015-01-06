package data.register

/**
 * Created by ryosuke on 14/11/18.
 */

//記号的に扱うべきか悩ましいところ
class ProgramCounter(p: Int) {

  private val limit = 0x0000FFFF
  var pc = p & limit

  def setPc(p: Int) = pc = p & limit

  override def toString(): String = pc.toString

}
