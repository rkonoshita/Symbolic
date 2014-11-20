package data.register

import z3.scala.{Z3AST, Z3Context}

/**
 * Created by ryosuke on 14/11/18.
 */
class PathCondition(c: Z3Context, p: Z3AST) {

  var path = p
  private final val ctx = c

  def set(p: Z3AST): Unit = path = ctx.mkOr(path, p)

}
