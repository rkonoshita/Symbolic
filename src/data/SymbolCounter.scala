package data

import data.register.{ConditionRegister, Memory, Register}
import main.Main
import symbol.CtxSymbol

import scala.collection.mutable.HashMap

/**
 * Created by rkonoshita on 15/01/21.
 */
class SymbolCounter(map: HashMap[String, Int]) {

  val counter = map

  def makeSymbol(num: Int, obj: String): CtxSymbol = {
    obj match {
      case "r" => up("r" + num, 32)
      case "m" => up("m" + num, 8)
      case "c" => up("c", 8)
    }
  }

  private def up(name: String, size: Int): CtxSymbol = {
    val ctx = Main.ctx
    if (!counter.contains(name)) counter += name -> -1
    counter(name) = counter(name) + 1
    new CtxSymbol(ctx.mkConst(name + "_" + counter(name), ctx.mkBVSort(size)))
  }
}
