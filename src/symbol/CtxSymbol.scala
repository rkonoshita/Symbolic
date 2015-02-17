package symbol

import base.Symbolic
import z3.scala.{Z3Sort, Z3AST}

/**
 * Created by rkonoshita on 14/11/26.
 */

//毎回ctx.~って書くのがめんどくさいから用意した！
class CtxSymbol(ast: Z3AST) {
  type T = Z3AST
  val symbol = ast
  val ctx = ast.context

  def this(ast: Int, size: Int) = this(Symbolic.ctx.mkInt(ast, Symbolic.ctx.mkBVSort(size)))

  def this(ast: Int, size: Z3Sort) = this(Symbolic.ctx.mkInt(ast, size))

  def this(name: String, size: Int) = this(Symbolic.ctx.mkConst(name, Symbolic.ctx.mkBVSort(size)))

  def +(s: CtxSymbol): CtxSymbol = this.+(s.symbol)

  def +(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAdd(symbol, s)).simpleify()

  def +(s: Int): CtxSymbol = this.+(ctx.mkInt(s, symbol.getSort))

  def -(s: CtxSymbol): CtxSymbol = this.-(s.symbol)

  def -(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSub(symbol, s)).simpleify()

  def -(s: Int): CtxSymbol = this.-(ctx.mkInt(s, symbol.getSort))

  def *(s: CtxSymbol): CtxSymbol = *(s.symbol)

  def *(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVMul(symbol, s)).simpleify()

  def *(s: Int): CtxSymbol = *(ctx.mkInt(s, symbol.getSort))

  def sdiv(s: CtxSymbol): CtxSymbol = sdiv(s.symbol)

  def sdiv(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSdiv(symbol, s)).simpleify()

  def sdiv(s: Int): CtxSymbol = sdiv(ctx.mkInt(s, symbol.getSort))

  def udiv(s: CtxSymbol): CtxSymbol = udiv(s.symbol)

  def udiv(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVUdiv(symbol, s)).simpleify()

  def udiv(s: Int): CtxSymbol = udiv(ctx.mkInt(s, symbol.getSort))

  def srem(s: CtxSymbol): CtxSymbol = srem(s.symbol)

  def srem(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSrem(symbol, s)).simpleify()

  def srem(s: Int): CtxSymbol = srem(ctx.mkInt(s, symbol.getSort))

  def urem(s: CtxSymbol): CtxSymbol = urem(s.symbol)

  def urem(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVUrem(symbol, s)).simpleify()

  def urem(s: Int): CtxSymbol = urem(ctx.mkInt(s, symbol.getSort))

  def &(s: CtxSymbol): CtxSymbol = &(s.symbol)

  def &(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAnd(symbol, s)).simpleify()

  def &(s: Int): CtxSymbol = &(ctx.mkInt(s, symbol.getSort))

  def |(s: CtxSymbol): CtxSymbol = |(s.symbol)

  def |(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVOr(symbol, s)).simpleify()

  def |(s: Int): CtxSymbol = |(ctx.mkInt(s, symbol.getSort))

  def ^(s: CtxSymbol): CtxSymbol = ^(s.symbol)

  def ^(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVXor(symbol, s)).simpleify()

  def ^(s: Int): CtxSymbol = ^(ctx.mkInt(s, symbol.getSort))

  def ~(): CtxSymbol = new CtxSymbol(ctx.mkBVNot(symbol)).simpleify()

  def neg: CtxSymbol = new CtxSymbol(ctx.mkBVNeg(symbol)).simpleify()

  def >>(s: CtxSymbol): CtxSymbol = >>(s.symbol)

  def >>(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAshr(symbol, s)).simpleify()

  def >>(s: Int): CtxSymbol = >>(ctx.mkInt(s, symbol.getSort))

  def <<(s: CtxSymbol): CtxSymbol = <<(s.symbol)

  def <<(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVShl(symbol, s)).simpleify()

  def <<(s: Int): CtxSymbol = <<(ctx.mkInt(s, symbol.getSort))

  def equal(s: CtxSymbol): CtxSymbol = equal(s.symbol)

  def equal(s: Int): CtxSymbol = equal(ctx.mkInt(s, symbol.getSort))

  def equal(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkEq(symbol, s)).simpleify()

  def not: CtxSymbol = new CtxSymbol(ctx.mkNot(symbol)).simpleify()

  def &&(s: CtxSymbol): CtxSymbol = &&(s.symbol)

  def &&(s: Int): CtxSymbol = &&(ctx.mkInt(s, symbol.getSort))

  def &&(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkAnd(symbol, s)).simpleify()

  def ||(s: CtxSymbol): CtxSymbol = ||(s.symbol)

  def ||(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkAnd(symbol, s)).simpleify()

  def ||(s: Int): CtxSymbol = ||(ctx.mkInt(s, symbol.getSort))

  def ^^(s: CtxSymbol): CtxSymbol = ^^(s.symbol)

  def ^^(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkXor(symbol, s)).simpleify()

  def ^^(s: Int): CtxSymbol = ^^(ctx.mkInt(s, symbol.getSort))

  def >=(s: CtxSymbol): CtxSymbol = >=(s.symbol)

  def >=(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSge(symbol, s)).simpleify()

  def >=(s: Int): CtxSymbol = >=(ctx.mkInt(s, symbol.getSort))

  def >(s: CtxSymbol): CtxSymbol = >(s.symbol)

  def >(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSgt(symbol, s)).simpleify()

  def >(s: Int): CtxSymbol = >(ctx.mkInt(s, symbol.getSort))

  def =<(s: CtxSymbol): CtxSymbol = =<(s.symbol)

  def =<(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSle(symbol, s)).simpleify()

  def =<(s: Int): CtxSymbol = =<(ctx.mkInt(s, symbol.getSort))

  def <(s: CtxSymbol): CtxSymbol = <(s.symbol)

  def <(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSlt(symbol, s)).simpleify()

  def <(s: Int): CtxSymbol = <(ctx.mkInt(s, symbol.getSort))

  def sextend(s: Int): CtxSymbol = new CtxSymbol(ctx.mkSignExt(s, symbol)).simpleify()

  def zextend(s: Int): CtxSymbol = new CtxSymbol(ctx.mkZeroExt(s, symbol)).simpleify()

  def extract(high: Int, low: Int): CtxSymbol = new CtxSymbol(ctx.mkExtract(high, low, symbol)).simpleify()

  def concat(s: CtxSymbol): CtxSymbol = concat(s.symbol)

  def concat(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkConcat(symbol, s)).simpleify()

  def bitClr(index: CtxSymbol): CtxSymbol = bitClr(index.symbol)

  def bitClr(index: Z3AST): CtxSymbol = &((new CtxSymbol(1, index.getSort) << index).~)

  def bitClr(index: Int): CtxSymbol = &(~(1 << index))

  def bitSet(index: CtxSymbol): CtxSymbol = bitSet(index.symbol)

  def bitSet(index: Z3AST): CtxSymbol = |(new CtxSymbol(1, index.getSort) << index)

  def bitSet(index: Int): CtxSymbol = |(1 << index)

  def bitNot(index: CtxSymbol): CtxSymbol = bitNot(index.symbol)

  def bitNot(index: Z3AST): CtxSymbol = bitStore(bitGet(index).~, index)

  def bitNot(index: Int): CtxSymbol = bitStore(bitGet(index).~, index)

  def bitGet(index: CtxSymbol): CtxSymbol = bitGet(index.symbol)

  def bitGet(index: Z3AST): CtxSymbol = (this >> index).extract(0, 0)

  def bitGet(index: Int): CtxSymbol = extract(index, index)

  def bitStore(s: CtxSymbol, index: CtxSymbol): CtxSymbol = bitStore(s.symbol, index.symbol)

  def bitStore(s: CtxSymbol, index: Z3AST): CtxSymbol = bitStore(s.symbol, index)

  def bitStore(s: CtxSymbol, index: Int): CtxSymbol = bitStore(s.symbol, index)

  def bitStore(s: Z3AST, index: Z3AST): CtxSymbol = {
    val ext = concat(new CtxSymbol(0, 8)) >> index
    val ans = (ext.extract(15, 9) concat s concat extract(7, 0)) << index
    ans.extract(15, 8)
  }

  def bitStore(s: Z3AST, index: Int): CtxSymbol = {
    index match {
      case 7 => new CtxSymbol(s) concat this.extract(6, 0)
      case 0 => this.extract(7, 1) concat s
      case _ => this.extract(7, index + 1) concat s concat this.extract(index - 1, 0)
    }
  }

  def store(index: CtxSymbol, store: CtxSymbol): CtxSymbol = this.store(index.symbol, store.symbol)

  def store(index: Z3AST, store: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkStore(symbol, index, store)).simpleify()

  def select(index: CtxSymbol): CtxSymbol = this.select(index.symbol)

  def select(index: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkSelect(symbol, index)).simpleify()

  def simpleify(): CtxSymbol = {
    val c = new CtxSymbol(ctx.simplifyAst(symbol))
    if (c.toString() == symbol.toString()) this
    else c
  }

  override def toString(): String = symbol.toString()

}