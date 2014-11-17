package symbol

import z3.scala.{Z3AST, Z3Context}

/**
 * Created by rkonoshita on 14/11/12.
 */
object NewSymbol {

  val num = 0

  def make(size: Int): Z3AST = {
    val ctx = new Z3Context
    ctx.mkConst("s" + num, ctx.mkBVSort(size))
  }

}
