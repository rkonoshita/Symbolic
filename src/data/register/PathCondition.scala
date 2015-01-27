package data.register

import base.Symbolic
import z3.scala.Z3AST

/**
 * Created by ryosuke on 14/11/18.
 */

//分岐がある場合は、ここに条件を追加していく
class PathCondition(p: Z3AST) {

  var path = p
  private val ctx = Symbolic.ctx

  def set(p: Z3AST): Unit =
    path =
      if (path == null) ctx.simplifyAst(p)
      else ctx.simplifyAst(ctx.mkAnd(path, p))

  override def toString():String =
    if (path == null) true.toString
    else path.toString()
}
