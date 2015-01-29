package data.register

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/12/05.
 */

//ROM領域
//基本的に記号実行中に書き換えてはいけない（はず）
class ROM(r: mutable.HashMap[Int, Byte]) {

  val rom = r
  private val limit = 0xFFFF

  def getByte(num: Int): Byte = rom(num & limit)

  def getWord(num: Int): Short = ((rom(num & limit) << 8) | (rom((num + 1) & limit) & 0xFF)).toShort

  def getLong(num: Int): Int = (rom(num & limit) << 24) | ((rom((num + 1) & limit) & 0xFF) << 16) | ((rom((num + 2) & limit) & 0xFF) << 8) | (rom((num + 3) & limit) & 0xFF)

}
