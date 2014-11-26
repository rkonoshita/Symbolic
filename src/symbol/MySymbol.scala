package symbol

import z3.scala.{Z3Context, Z3AST}

/**
 * Created by rkonoshita on 14/11/26.
 */
trait MySymbol {
  type T
  val symbol: T
}

class CtxSymbol(ast: Z3AST) extends MySymbol {
  type T = Z3AST
  val symbol = ast
  val ctx = ast.context

  def &(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) &(s.asInstanceOf[IntSymbol].symbol)
    else &(s.asInstanceOf[CtxSymbol].symbol)

  def &(s: Z3AST): MySymbol = new CtxSymbol(ctx.mkBVAnd(symbol, s))

  def &(s: Int): MySymbol = new CtxSymbol(ctx.mkBVAnd(symbol, ctx.mkInt(s, symbol.getSort)))

  def >>(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) >>(s.asInstanceOf[IntSymbol].symbol)
    else >>(s.asInstanceOf[CtxSymbol].symbol)

  def >>(s: Z3AST): MySymbol = new CtxSymbol(ctx.mkBVAshr(symbol, s))

  def >>(s: Int): MySymbol = new CtxSymbol(ctx.mkBVAshr(symbol, ctx.mkInt(s, symbol.getSort)))
}

class IntSymbol(ast: Int) extends MySymbol {
  type T = Int
  val symbol = ast

  def &(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) &(s.asInstanceOf[IntSymbol].symbol)
    else &(s.asInstanceOf[CtxSymbol].symbol)

  def &(s: Z3AST): MySymbol = new CtxSymbol(s.context.mkBVAnd(s.context.mkInt(symbol, s.getSort), s))

  def &(s: Int): MySymbol = new IntSymbol(symbol & s)

  def >>(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) >>(s.asInstanceOf[IntSymbol].symbol)
    else >>(s.asInstanceOf[CtxSymbol].symbol)

  def >>(s: Z3AST): MySymbol = new CtxSymbol(s.context.mkBVAshr(s.context.mkInt(symbol, s.getSort), s))

  def >>(s: Int): MySymbol = new IntSymbol(symbol >> s)
}