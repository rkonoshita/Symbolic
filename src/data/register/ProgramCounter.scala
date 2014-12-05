package data.register

import main.Main
import symbol.CtxSymbol
import z3.scala.Z3Context

import scala.collection.mutable.ArrayBuffer

/**
 * Created by ryosuke on 14/11/18.
 */

//記号的に扱うべきか悩ましいところ
class ProgramCounter(p: Int) {

  var pc = p
  private val limit = 0x0000FFFF
//  private val limit = 0x00FFFFFF //アドバンスモード

}
