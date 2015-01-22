package data.register

import main.Main
import z3.scala.Z3AST

/**
 * Created by ryosuke on 14/11/18.
 */

//分岐がある場合は、ここに条件を追加していく
class PathCondition(p: Z3AST) {

  var path = p
  private val ctx = Main.ctx

  def set(p: Z3AST): Unit =
    path =
      if (path == null) ctx.simplifyAst(p)
      else ctx.simplifyAst(ctx.mkAnd(path, p))

}
