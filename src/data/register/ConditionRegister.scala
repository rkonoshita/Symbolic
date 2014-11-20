package data.register

import z3.scala.{Z3Context, Z3AST}

/**
 * Created by ryosuke on 14/11/18.
 */
class ConditionRegister(ct: Z3Context, c: Z3AST) {

  val ccr = c
  private val ctx = ct

}
