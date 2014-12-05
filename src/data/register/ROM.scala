package data.register

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/12/05.
 */
class ROM(r: mutable.HashMap[Int, Byte]) {

  val rom = r

  def readByte(num: Int): Byte = rom(num)

  def readWord(num: Int): Short = ((rom(num) << 8) | (rom(num + 1))).toShort

  def readLong(num: Int): Int = (rom(num) << 24) | (rom(num + 1) << 16) | (rom(num + 2) << 8) | (rom(num + 3))

}
