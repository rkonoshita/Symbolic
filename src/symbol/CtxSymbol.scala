package symbol

import z3.scala.{Z3Context, Z3AST}

/**
 * Created by rkonoshita on 14/11/26.
 */

//記号計算を直感的に記述できるように工夫している

class CtxSymbol(ast: Z3AST) {
  val symbol = ast
  val ctx = ast.context

  def this(ctx: Z3Context, ast: Int, size: Int) = this(ctx.mkInt(ast, ctx.mkBVSort(size)))

  def +(s: CtxSymbol): CtxSymbol = this.+(s.symbol)

  def +(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAdd(symbol, s))

  def +(s: Int): CtxSymbol = this.+(ctx.mkInt(s, symbol.getSort))

  def -(s: CtxSymbol): CtxSymbol = this.-(s.symbol)

  def -(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSub(symbol, s))

  def -(s: Int): CtxSymbol = this.-(ctx.mkInt(s, symbol.getSort))

  def &(s: CtxSymbol): CtxSymbol = &(s.symbol)

  def &(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAnd(symbol, s))

  def &(s: Int): CtxSymbol = &(ctx.mkInt(s, symbol.getSort))

  def |(s: CtxSymbol): CtxSymbol = |(s.symbol)

  def |(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVOr(symbol, s))

  def |(s: Int): CtxSymbol = |(ctx.mkInt(s, symbol.getSort))

  def ^(s: CtxSymbol): CtxSymbol = ^(s.symbol)

  def ^(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVXor(symbol, s))

  def ^(s: Int): CtxSymbol = ^(ctx.mkInt(s, symbol.getSort))

  def ~(): CtxSymbol = new CtxSymbol(ctx.mkBVNot(symbol))

  def neg: CtxSymbol = new CtxSymbol(ctx.mkBVNeg(symbol))

  def >>(s: CtxSymbol): CtxSymbol = >>(s.symbol)

  def >>(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAshr(symbol, s))

  def >>(s: Int): CtxSymbol = >>(ctx.mkInt(s, symbol.getSort))

  def <<(s: CtxSymbol): CtxSymbol = <<(s.symbol)

  def <<(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVShl(symbol, s))

  def <<(s: Int): CtxSymbol = <<(ctx.mkInt(s, symbol.getSort))

  def eq(s: CtxSymbol): CtxSymbol = eq(s.symbol)

  def eq(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkEq(symbol, s))

  def eq(s: Int): CtxSymbol = eq(ctx.mkInt(s, symbol.getSort))

  def not: CtxSymbol = new CtxSymbol(ctx.mkNot(symbol))

  def &&(s: CtxSymbol): CtxSymbol = &&(s.symbol)

  def &&(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkAnd(symbol, s))

  def &&(s: Int): CtxSymbol = &&(ctx.mkInt(s, symbol.getSort))

  def ||(s: CtxSymbol): CtxSymbol = ||(s.symbol)

  def ||(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkOr(symbol, s))

  def ||(s: Int): CtxSymbol = ||(ctx.mkInt(s, symbol.getSort))

  def >=(s: CtxSymbol): CtxSymbol = >=(s.symbol)

  def >=(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSge(symbol, s))

  def >=(s: Int): CtxSymbol = >=(ctx.mkInt(s, symbol.getSort))

  def <(s: CtxSymbol): CtxSymbol = <(s.symbol)

  def <(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSlt(symbol, s))

  def <(s: Int): CtxSymbol = <(ctx.mkInt(s, symbol.getSort))

  def extract(high: Int, low: Int): CtxSymbol = new CtxSymbol(ctx.mkExtract(high, low, symbol))

  def ::(s: CtxSymbol): CtxSymbol = new CtxSymbol(ctx.mkConcat(symbol, s.symbol))

  def bitset(s: CtxSymbol): CtxSymbol = bitset(s.symbol)

  def bitset(s: Z3AST): CtxSymbol = |(new CtxSymbol(ctx.mkInt(1, s.getSort)) << s)

  def bitset(s: Int): CtxSymbol = |(1 << s)

  override def toString(): String = symbol.toString
}
