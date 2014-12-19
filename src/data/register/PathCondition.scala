package data.register

import main.Main
import z3.scala.{Z3AST, Z3Context}

/**
 * Created by ryosuke on 14/11/18.
 */

//分岐がある場合は、ここに条件を追加していく
class PathCondition(p: Z3AST) {

  var path = p
  private val ctx = Main.ctx

  def set(p: Z3AST): Unit =
    if (path == null) path = p
    else path = ctx.mkAnd(path, p)

}
