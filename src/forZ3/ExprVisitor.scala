package forZ3

import z3.scala.{Z3AST, Z3Context}

/**
 * Created by ryosuke on 14/11/15.
 */
class ExprVisitor(c: Z3Context, s: Int) {

  val ctx = c
  val size = s

  def visit(ast: AST): Z3AST = {
    ast match {
      case andLog(left, right) => ctx.mkAnd(visit(left), visit(right))
      case orLog(left, right) => ctx.mkOr(visit(left), visit(right))
      case notLog(item) => ctx.mkNot(visit(item))
      case equal(left, right) => ctx.mkEq(visit(left), visit(right))
      case bvSge(left, right) => ctx.mkBVSge(visit(left), visit(right))
      case bvSgt(left, right) => ctx.mkBVSgt(visit(left), visit(right))
      case bvSle(left, right) => ctx.mkBVSle(visit(left), visit(right))
      case bvSlt(left, right) => ctx.mkBVSlt(visit(left), visit(right))
      case bvUge(left, right) => ctx.mkBVUge(visit(left), visit(right))
      case bvUgt(left, right) => ctx.mkBVUgt(visit(left), visit(right))
      case bvUle(left, right) => ctx.mkBVUle(visit(left), visit(right))
      case bvUlt(left, right) => ctx.mkBVUlt(visit(left), visit(right))
      case bvAdd(left, right) => ctx.mkBVAdd(visit(left), visit(right))
      case bvSub(left, right) => ctx.mkBVSub(visit(left), visit(right))
      case bvMul(left, right) => ctx.mkBVMul(visit(left), visit(right))
      case bvSdiv(left, right) => ctx.mkBVSdiv(visit(left), visit(right))
      case bvUdiv(left, right) => ctx.mkBVUdiv(visit(left), visit(right))
      case bvAnd(left, right) => ctx.mkBVAnd(visit(left), visit(right))
      case bvOr(left, right) => ctx.mkBVOr(visit(left), visit(right))
      case bvXor(left, right) => ctx.mkBVXor(visit(left), visit(right))
      case bvAshr(left, right) => ctx.mkBVAshr(visit(left), visit(right))
      case bvLshr(left, right) => ctx.mkBVLshr(visit(left), visit(right))
      case bvShl(left, right) => ctx.mkBVShl(visit(left), visit(right))
      case bvNot(item) => ctx.mkBVNot(visit(item))
      case bvNeg(item) => ctx.mkBVNeg(visit(item))
      case mkInt(num) => ctx.mkInt(num, ctx.mkBVSort(size))
      case mkSymbol(str) => ctx.mkConst(str, ctx.mkBVSort(size))
    }
  }

}
