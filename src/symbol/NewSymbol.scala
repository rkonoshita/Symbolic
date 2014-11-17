package symbol

import main.Main
import z3.scala.{Z3AST, Z3Context}

/**
 * Created by rkonoshita on 14/11/12.
 */
object NewSymbol {

  var num = -1

  def make(size: Int): Z3AST = {
    num += 1
    //val ctx = new Z3Context
    Main.ctx.mkConst("s" + num, Main.ctx.mkBVSort(size))
  }

}
