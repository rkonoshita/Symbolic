package data.register

import z3.scala.{Z3AST, Z3Context}

/**
 * Created by ryosuke on 14/11/18.
 */

//分岐がある場合は、ここに条件を追加していく
class PathCondition(c: Z3Context, p: Z3AST) {

  var path = p
  private final val ctx = c

  def set(p: Z3AST): Unit =
    if (path == null) path = p
    else path = ctx.mkOr(path, p)

  def this(c: Z3Context) = this(c, null)
}
