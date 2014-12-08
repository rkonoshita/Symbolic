package symbol

import z3.scala.{Z3Context, Z3AST}

/**
 * Created by rkonoshita on 14/11/26.
 */

//記号計算を直感的に記述できるように工夫している
trait MySymbol {
  type T
  val symbol: T

  def +(s: MySymbol): MySymbol

  def +(s: Int): MySymbol

  def -(s: MySymbol): MySymbol

  def -(s: Int): MySymbol

  def |(s: MySymbol): MySymbol

  def |(s: Int): MySymbol

  def &(s: MySymbol): MySymbol

  def &(s: Int): MySymbol

  def ^(s: MySymbol): MySymbol

  def ^(s: Int): MySymbol

  def ~(): MySymbol

  def neg: MySymbol

  def <<(s: MySymbol): MySymbol

  def <<(s: Int): MySymbol

  def >>(s: MySymbol): MySymbol

  def >>(s: Int): MySymbol

  def eq(s: MySymbol): MySymbol

  def eq(s: Z3AST): MySymbol

  def >=(s: MySymbol): MySymbol

  def >=(s: Z3AST): MySymbol

  def <(s: MySymbol): MySymbol

  def <(s: Z3AST): MySymbol

  def &&(s: MySymbol): MySymbol

  def ||(s: MySymbol): MySymbol

  def bitset(s: MySymbol): MySymbol

  def bitset(s: Int): MySymbol
}

class CtxSymbol(ast: Z3AST) extends MySymbol {
  type T = Z3AST
  val symbol = ast
  val ctx = ast.context

  def this(ctx: Z3Context, ast: Int, size: Int) = this(ctx.mkInt(ast, ctx.mkBVSort(size)))

  def +(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) this.+(s.asInstanceOf[IntSymbol].symbol)
    else this.+(s.asInstanceOf[CtxSymbol].symbol)

  def +(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAdd(symbol, s))

  def +(s: Int): CtxSymbol = this.+(ctx.mkInt(s, symbol.getSort))

  def -(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) this.-(s.asInstanceOf[IntSymbol].symbol)
    else this.-(s.asInstanceOf[CtxSymbol].symbol)

  def -(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSub(symbol, s))

  def -(s: Int): CtxSymbol = this.-(ctx.mkInt(s, symbol.getSort))

  def &(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) &(s.asInstanceOf[IntSymbol].symbol)
    else &(s.asInstanceOf[CtxSymbol].symbol)

  def &(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAnd(symbol, s))

  def &(s: Int): CtxSymbol = &(ctx.mkInt(s, symbol.getSort))

  def |(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) |(s.asInstanceOf[IntSymbol].symbol)
    else |(s.asInstanceOf[CtxSymbol].symbol)

  def |(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVOr(symbol, s))

  def |(s: Int): CtxSymbol = |(ctx.mkInt(s, symbol.getSort))

  def ^(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) ^(s.asInstanceOf[IntSymbol].symbol)
    else ^(s.asInstanceOf[CtxSymbol].symbol)

  def ^(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVXor(symbol, s))

  def ^(s: Int): CtxSymbol = ^(ctx.mkInt(s, symbol.getSort))

  def ~(): CtxSymbol = new CtxSymbol(ctx.mkBVNot(symbol))

  def neg: CtxSymbol = new CtxSymbol(ctx.mkBVNeg(symbol))

  def >>(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) >>(s.asInstanceOf[IntSymbol].symbol)
    else >>(s.asInstanceOf[CtxSymbol].symbol)

  def >>(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAshr(symbol, s))

  def >>(s: Int): CtxSymbol = >>(ctx.mkInt(s, symbol.getSort))

  def <<(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) <<(s.asInstanceOf[IntSymbol].symbol)
    else <<(s.asInstanceOf[CtxSymbol].symbol)

  def <<(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVShl(symbol, s))

  def <<(s: Int): CtxSymbol = <<(ctx.mkInt(s, symbol.getSort))

  def eq(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) eq(s.asInstanceOf[IntSymbol].symbol)
    else eq(s.asInstanceOf[CtxSymbol].symbol)

  def eq(s: Int): CtxSymbol = eq(ctx.mkInt(s, symbol.getSort))

  def eq(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkEq(symbol, s))

  def not: CtxSymbol = new CtxSymbol(ctx.mkNot(symbol))

  def &&(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) &&(s.asInstanceOf[IntSymbol].symbol)
    else &&(s.asInstanceOf[CtxSymbol].symbol)

  def &&(s: Int): CtxSymbol = &&(ctx.mkInt(s, symbol.getSort))

  def &&(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkAnd(symbol, s))

  def ||(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) ||(s.asInstanceOf[IntSymbol].symbol)
    else ||(s.asInstanceOf[CtxSymbol].symbol)

  def ||(s: Int): CtxSymbol = ||(ctx.mkInt(s, symbol.getSort))

  def ||(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkAnd(symbol, s))

  def >=(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) >=(s.asInstanceOf[IntSymbol].symbol)
    else >=(s.asInstanceOf[CtxSymbol].symbol)

  def >=(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSge(symbol, s))

  def >=(s: Int): CtxSymbol = >=(ctx.mkInt(s, symbol.getSort))

  def <(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) >=(s.asInstanceOf[IntSymbol].symbol)
    else >=(s.asInstanceOf[CtxSymbol].symbol)

  def <(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSlt(symbol, s))

  def <(s: Int): CtxSymbol = <(ctx.mkInt(s, symbol.getSort))

  def extract(high: Int, low: Int): CtxSymbol = new CtxSymbol(ctx.mkExtract(high, low, symbol))

  def ::(s: CtxSymbol): CtxSymbol = new CtxSymbol(ctx.mkConcat(symbol, s.symbol))

  def bitset(s: MySymbol): CtxSymbol =
    if (s.isInstanceOf[IntSymbol]) bitset(s.asInstanceOf[IntSymbol].symbol)
    else bitset(s.asInstanceOf[CtxSymbol].symbol)

  def bitset(s: Z3AST): CtxSymbol = |(new IntSymbol(1) << s)

  def bitset(s: Int): CtxSymbol = |(1 << s)

  override def toString(): String = symbol.toString
}

class IntSymbol(ast: Int) extends MySymbol {
  type T = Int
  val symbol = ast

  def +(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) this.+(s.asInstanceOf[IntSymbol].symbol)
    else this.+(s.asInstanceOf[CtxSymbol].symbol)

  def +(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVAdd(s.context.mkInt(symbol, s.getSort), s))

  def +(s: Int): IntSymbol = new IntSymbol(symbol + s)

  def -(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) this.-(s.asInstanceOf[IntSymbol].symbol)
    else this.-(s.asInstanceOf[CtxSymbol].symbol)

  def -(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVSub(s.context.mkInt(symbol, s.getSort), s))

  def -(s: Int): IntSymbol = new IntSymbol(symbol - s)

  def &(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) &(s.asInstanceOf[IntSymbol].symbol)
    else &(s.asInstanceOf[CtxSymbol].symbol)

  def &(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVAnd(s.context.mkInt(symbol, s.getSort), s))

  def &(s: Int): IntSymbol = new IntSymbol(symbol & s)

  def |(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) |(s.asInstanceOf[IntSymbol].symbol)
    else |(s.asInstanceOf[CtxSymbol].symbol)

  def |(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVOr(s.context.mkInt(symbol, s.getSort), s))

  def |(s: Int): IntSymbol = new IntSymbol(symbol | s)

  def ^(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) ^(s.asInstanceOf[IntSymbol].symbol)
    else ^(s.asInstanceOf[CtxSymbol].symbol)

  def ^(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVXor(s.context.mkInt(symbol, s.getSort), s))

  def ^(s: Int): IntSymbol = new IntSymbol(symbol ^ s)

  def ~(): IntSymbol = new IntSymbol(~symbol)

  def neg: IntSymbol = this.~ + 1

  def >>(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) >>(s.asInstanceOf[IntSymbol].symbol)
    else >>(s.asInstanceOf[CtxSymbol].symbol)

  def >>(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVAshr(s.context.mkInt(symbol, s.getSort), s))

  def >>(s: Int): IntSymbol = new IntSymbol(symbol >> s)

  def <<(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) <<(s.asInstanceOf[IntSymbol].symbol)
    else <<(s.asInstanceOf[CtxSymbol].symbol)

  def <<(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVShl(s.context.mkInt(symbol, s.getSort), s))

  def <<(s: Int): IntSymbol = new IntSymbol(symbol << s)

  def eq(s: MySymbol): MySymbol = eq(s.asInstanceOf[CtxSymbol].symbol)

  def eq(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkEq(s.context.mkInt(symbol, s.getSort), s))

  def eq(s: Int): Boolean = symbol == s

  def &&(s: MySymbol): MySymbol = &&(s.asInstanceOf[CtxSymbol].symbol)

  def &&(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkAnd(s.context.mkInt(symbol, s.getSort), s))

  def ||(s: MySymbol): MySymbol = ||(s.asInstanceOf[CtxSymbol].symbol)

  def ||(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkOr(s.context.mkInt(symbol, s.getSort), s))

  def >=(s: MySymbol): MySymbol = >=(s.asInstanceOf[CtxSymbol].symbol)

  def >=(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVSge(s.context.mkInt(symbol, s.getSort), s))

  def <(s: MySymbol): CtxSymbol = <(s.asInstanceOf[CtxSymbol].symbol)

  def <(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVSlt(s.context.mkInt(symbol, s.getSort), s))

  def <(s: Int): Boolean = symbol < s

  def bitset(s: MySymbol): MySymbol =
    if (s.isInstanceOf[IntSymbol]) bitset(s.asInstanceOf[IntSymbol].symbol)
    else bitset(s.asInstanceOf[CtxSymbol].symbol)

  def bitset(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVOr(s.context.mkInt(symbol, s.getSort), s.context.mkBVShl(s.context.mkInt(1, s.getSort), s)))

  def bitset(s: Int): IntSymbol = |(1 << s)

  override def toString(): String = symbol.toString
}