package symbol

import base.Symbolic
import z3.scala.{Z3Sort, Z3AST}

/**
 * Created by rkonoshita on 14/11/26.
 */

class CtxSymbol(ast: Z3AST) {
  type T = Z3AST
  val symbol = ast
  val ctx = ast.context

  def this(ast: Int, size: Int) = this(Symbolic.ctx.mkInt(ast, Symbolic.ctx.mkBVSort(size)))

  def this(ast: Int, size: Z3Sort) = this(Symbolic.ctx.mkInt(ast, size))

  def +(s: CtxSymbol): CtxSymbol = this.+(s.symbol)

  def +(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAdd(symbol, s))

  def +(s: Int): CtxSymbol = this.+(ctx.mkInt(s, symbol.getSort))

  def -(s: CtxSymbol): CtxSymbol = this.-(s.symbol)

  def -(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSub(symbol, s))

  def -(s: Int): CtxSymbol = this.-(ctx.mkInt(s, symbol.getSort))

  def *(s: CtxSymbol): CtxSymbol = *(s.symbol)

  def *(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVMul(symbol, s))

  def *(s: Int): CtxSymbol = *(ctx.mkInt(s, symbol.getSort))

  def sdiv(s: CtxSymbol): CtxSymbol = sdiv(s.symbol)

  def sdiv(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSdiv(symbol, s))

  def sdiv(s: Int): CtxSymbol = sdiv(ctx.mkInt(s, symbol.getSort))

  def udiv(s: CtxSymbol): CtxSymbol = udiv(s.symbol)

  def udiv(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVUdiv(symbol, s))

  def udiv(s: Int): CtxSymbol = udiv(ctx.mkInt(s, symbol.getSort))

  def srem(s: CtxSymbol): CtxSymbol = srem(s.symbol)

  def srem(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSrem(symbol, s))

  def srem(s: Int): CtxSymbol = srem(ctx.mkInt(s, symbol.getSort))

  def urem(s: CtxSymbol): CtxSymbol = urem(s.symbol)

  def urem(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVUrem(symbol, s))

  def urem(s: Int): CtxSymbol = urem(ctx.mkInt(s, symbol.getSort))

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

  def equal(s: CtxSymbol): CtxSymbol = equal(s.symbol)

  def equal(s: Int): CtxSymbol = equal(ctx.mkInt(s, symbol.getSort))

  def equal(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkEq(symbol, s))

  def not: CtxSymbol = new CtxSymbol(ctx.mkNot(symbol))

  def &&(s: CtxSymbol): CtxSymbol = &&(s.symbol)

  def &&(s: Int): CtxSymbol = &&(ctx.mkInt(s, symbol.getSort))

  def &&(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkAnd(symbol, s))

  def ||(s: CtxSymbol): CtxSymbol = ||(s.symbol)

  def ||(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkAnd(symbol, s))

  def ||(s: Int): CtxSymbol = ||(ctx.mkInt(s, symbol.getSort))

  def ^^(s: CtxSymbol): CtxSymbol = ^^(s.symbol)

  def ^^(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkXor(symbol, s))

  def ^^(s: Int): CtxSymbol = ^^(ctx.mkInt(s, symbol.getSort))

  def >=(s: CtxSymbol): CtxSymbol = >=(s.symbol)

  def >=(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSge(symbol, s))

  def >=(s: Int): CtxSymbol = >=(ctx.mkInt(s, symbol.getSort))

  def >(s: CtxSymbol): CtxSymbol = >(s.symbol)

  def >(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSgt(symbol, s))

  def >(s: Int): CtxSymbol = >(ctx.mkInt(s, symbol.getSort))

  def <(s: CtxSymbol): CtxSymbol = <(s.symbol)

  def <(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSlt(symbol, s))

  def <(s: Int): CtxSymbol = <(ctx.mkInt(s, symbol.getSort))

  def sextend(s: Int): CtxSymbol = new CtxSymbol(ctx.mkSignExt(s, symbol))

  def zextend(s: Int): CtxSymbol = new CtxSymbol(ctx.mkZeroExt(s, symbol))

  def extract(high: Int, low: Int): CtxSymbol = new CtxSymbol(ctx.mkExtract(high, low, symbol))

  def concat(s: CtxSymbol): CtxSymbol = new CtxSymbol(ctx.mkConcat(symbol, s.symbol))

  def bitclr(s: CtxSymbol): CtxSymbol = bitclr(s.symbol)

  def bitclr(s: Z3AST): CtxSymbol = &((new CtxSymbol(1, s.getSort) << s).~)

  def bitclr(s: Int): CtxSymbol = &(~(1 << s))

  def bitset(s: CtxSymbol): CtxSymbol = bitset(s.symbol)

  def bitset(s: Z3AST): CtxSymbol = |(new CtxSymbol(1, s.getSort) << s)

  def bitset(s: Int): CtxSymbol = |(1 << s)

  def store(index: CtxSymbol, store: CtxSymbol): CtxSymbol = this.store(index.symbol, store.symbol)

  def store(index: Z3AST, store: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkStore(symbol, index, store))

  def select(index: CtxSymbol): CtxSymbol = this.select(index.symbol)

  def select(index: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkSelect(symbol, index))

  def simpleify(): CtxSymbol = {
    val c = new CtxSymbol(ctx.simplifyAst(symbol))
    if (c.toString() == symbol.toString()) this
    else c
  }

  override def toString(): String = symbol.toString()

}