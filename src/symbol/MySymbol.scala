package symbol

import main.Main
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

  def *(s: MySymbol): MySymbol

  def *(s: Int): MySymbol

  def sdiv(s: MySymbol): MySymbol

  def sdiv(s: Int): MySymbol

  def udiv(s: MySymbol): MySymbol

  def udiv(s: Int): MySymbol

  def srem(s: MySymbol): MySymbol

  def srem(s: Int): MySymbol

  def urem(s: MySymbol): MySymbol

  def urem(s: Int): MySymbol

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

  def >=(s: MySymbol): MySymbol

  def <(s: MySymbol): MySymbol

  def &&(s: MySymbol): MySymbol

  def ||(s: MySymbol): MySymbol

  def ^^(s: MySymbol): MySymbol

  def bitclr(s: MySymbol): MySymbol

  def bitclr(s: Int): MySymbol

  def bitset(s: MySymbol): MySymbol

  def bitset(s: Int): MySymbol
}

class CtxSymbol(ast: Z3AST) extends MySymbol {
  type T = Z3AST
  val symbol = ast
  val ctx = ast.context

  def this(ast: Int, size: Int) = this(Main.ctx.mkInt(ast, Main.ctx.mkBVSort(size)))

  def this(ast: IntSymbol, size: Int) = this(ast.symbol, size)

  def +(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => this.+(sym.symbol)
    case sym: CtxSymbol => this.+(sym.symbol)
  }

  def +(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAdd(symbol, s))

  def +(s: Int): CtxSymbol = this.+(ctx.mkInt(s, symbol.getSort))

  def -(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => this.-(sym.symbol)
    case sym: CtxSymbol => this.-(sym.symbol)
  }

  def -(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSub(symbol, s))

  def -(s: Int): CtxSymbol = this.-(ctx.mkInt(s, symbol.getSort))

  def *(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => *(sym.symbol)
    case sym: CtxSymbol => *(sym.symbol)
  }

  def *(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVMul(symbol, s))

  def *(s: Int): CtxSymbol = *(ctx.mkInt(s, symbol.getSort))

  def sdiv(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => sdiv(sym.symbol)
    case sym: CtxSymbol => sdiv(sym.symbol)
  }

  def sdiv(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSdiv(symbol, s))

  def sdiv(s: Int): CtxSymbol = sdiv(ctx.mkInt(s, symbol.getSort))

  def udiv(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => udiv(sym.symbol)
    case sym: CtxSymbol => udiv(sym.symbol)
  }

  def udiv(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVUdiv(symbol, s))

  def udiv(s: Int): CtxSymbol = udiv(ctx.mkInt(s, symbol.getSort))

  def srem(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => srem(sym.symbol)
    case sym: CtxSymbol => srem(sym.symbol)
  }

  def srem(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSrem(symbol, s))

  def srem(s: Int): CtxSymbol = srem(ctx.mkInt(s, symbol.getSort))

  def urem(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => urem(sym.symbol)
    case sym: CtxSymbol => urem(sym.symbol)
  }

  def urem(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVUrem(symbol, s))

  def urem(s: Int): CtxSymbol = urem(ctx.mkInt(s, symbol.getSort))

  def &(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => &(sym.symbol)
    case sym: CtxSymbol => &(sym.symbol)
  }

  def &(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAnd(symbol, s))

  def &(s: Int): CtxSymbol = &(ctx.mkInt(s, symbol.getSort))

  def |(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => |(sym.symbol)
    case sym: CtxSymbol => |(sym.symbol)
  }

  def |(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVOr(symbol, s))

  def |(s: Int): CtxSymbol = |(ctx.mkInt(s, symbol.getSort))

  def ^(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => ^(sym.symbol)
    case sym: CtxSymbol => ^(sym.symbol)
  }

  def ^(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVXor(symbol, s))

  def ^(s: Int): CtxSymbol = ^(ctx.mkInt(s, symbol.getSort))

  def ~(): CtxSymbol = new CtxSymbol(ctx.mkBVNot(symbol))

  def neg: CtxSymbol = new CtxSymbol(ctx.mkBVNeg(symbol))

  def >>(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => >>(sym.symbol)
    case sym: CtxSymbol => >>(sym.symbol)
  }

  def >>(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVAshr(symbol, s))

  def >>(s: Int): CtxSymbol = >>(ctx.mkInt(s, symbol.getSort))

  def <<(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => <<(sym.symbol)
    case sym: CtxSymbol => <<(sym.symbol)
  }

  def <<(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVShl(symbol, s))

  def <<(s: Int): CtxSymbol = <<(ctx.mkInt(s, symbol.getSort))

  def eq(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => eq(sym.symbol)
    case sym: CtxSymbol => eq(sym.symbol)
  }

  def eq(s: Int): CtxSymbol = eq(ctx.mkInt(s, symbol.getSort))

  def eq(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkEq(symbol, s))

  def not: CtxSymbol = new CtxSymbol(ctx.mkNot(symbol))

  def &&(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => &&(sym.symbol)
    case sym: CtxSymbol => &&(sym.symbol)
  }

  def &&(s: Int): CtxSymbol = &&(ctx.mkInt(s, symbol.getSort))

  def &&(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkAnd(symbol, s))

  def ||(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => ||(sym.symbol)
    case sym: CtxSymbol => ||(sym.symbol)
  }

  def ||(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkAnd(symbol, s))

  def ||(s: Int): CtxSymbol = ||(ctx.mkInt(s, symbol.getSort))

  def ^^(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => ^^(sym.symbol)
    case sym: CtxSymbol => ^^(sym.symbol)
  }

  def ^^(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkXor(symbol, s))

  def ^^(s: Int): CtxSymbol = ^^(ctx.mkInt(s, symbol.getSort))

  def >=(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => >=(sym.symbol)
    case sym: CtxSymbol => >=(sym.symbol)
  }

  def >=(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSge(symbol, s))

  def >=(s: Int): CtxSymbol = >=(ctx.mkInt(s, symbol.getSort))

  def <(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => <(sym.symbol)
    case sym: CtxSymbol => <(sym.symbol)
  }

  def <(s: Z3AST): CtxSymbol = new CtxSymbol(ctx.mkBVSlt(symbol, s))

  def <(s: Int): CtxSymbol = <(ctx.mkInt(s, symbol.getSort))

  def sextend(s: Int): CtxSymbol = new CtxSymbol(ctx.mkSignExt(s, symbol))

  def zextend(s: Int): CtxSymbol = new CtxSymbol(ctx.mkZeroExt(s, symbol))

  def extract(high: Int, low: Int): CtxSymbol = new CtxSymbol(ctx.mkExtract(high, low, symbol))

  def concat(s: CtxSymbol): CtxSymbol = new CtxSymbol(ctx.mkConcat(symbol, s.symbol))

  def bitclr(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => bitclr(sym.symbol)
    case sym: CtxSymbol => bitclr(sym.symbol)
  }

  def bitclr(s: Z3AST): CtxSymbol = &((new IntSymbol(1) << s).~)

  def bitclr(s: Int): CtxSymbol = &(~(1 << s))

  def bitset(s: MySymbol): CtxSymbol = s match {
    case sym: IntSymbol => bitset(sym.symbol)
    case sym: CtxSymbol => bitset(sym.symbol)
  }

  def bitset(s: Z3AST): CtxSymbol = |(new IntSymbol(1) << s)

  def bitset(s: Int): CtxSymbol = |(1 << s)

  override def toString(): String = symbol.toString
}

class IntSymbol(ast: Int) extends MySymbol {
  type T = Int
  val symbol = ast

  def +(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => this.+(sym.symbol)
    case sym: CtxSymbol => this.+(sym.symbol)
  }

  def +(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVAdd(s.context.mkInt(symbol, s.getSort), s))

  def +(s: Int): IntSymbol = new IntSymbol(symbol + s)

  def -(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => |(sym.symbol)
    case sym: CtxSymbol => |(sym.symbol)
  }

  def -(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVSub(s.context.mkInt(symbol, s.getSort), s))

  def -(s: Int): IntSymbol = new IntSymbol(symbol - s)

  def *(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => *(sym.symbol)
    case sym: CtxSymbol => *(sym.symbol)
  }

  def *(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVMul(s.context.mkInt(symbol, s.getSort), s))

  def *(s: Int): IntSymbol = new IntSymbol(symbol * s)

  def sdiv(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => sdiv(sym.symbol)
    case sym: CtxSymbol => sdiv(sym.symbol)
  }

  def sdiv(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVSdiv(s.context.mkInt(symbol, s.getSort), s))

  def sdiv(s: Int): IntSymbol = new IntSymbol(symbol / s)

  def udiv(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => udiv(sym.symbol)
    case sym: CtxSymbol => udiv(sym.symbol)
  }

  def udiv(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVUdiv(s.context.mkInt(symbol, s.getSort), s))

  def udiv(s: Int): IntSymbol = new IntSymbol(Integer.divideUnsigned(symbol, s))

  def srem(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => srem(sym.symbol)
    case sym: CtxSymbol => srem(sym.symbol)
  }

  def srem(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVSrem(s.context.mkInt(symbol, s.getSort), s))

  def srem(s: Int): IntSymbol = new IntSymbol(symbol % s)

  def urem(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => urem(sym.symbol)
    case sym: CtxSymbol => urem(sym.symbol)
  }

  def urem(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVUrem(s.context.mkInt(symbol, s.getSort), s))

  def urem(s: Int): IntSymbol = new IntSymbol(Integer.remainderUnsigned(symbol, s))

  def &(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => &(sym.symbol)
    case sym: CtxSymbol => &(sym.symbol)
  }

  def &(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVAnd(s.context.mkInt(symbol, s.getSort), s))

  def &(s: Int): IntSymbol = new IntSymbol(symbol & s)

  def |(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => |(sym.symbol)
    case sym: CtxSymbol => |(sym.symbol)
  }

  def |(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVOr(s.context.mkInt(symbol, s.getSort), s))

  def |(s: Int): IntSymbol = new IntSymbol(symbol | s)

  def ^(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => ^(sym.symbol)
    case sym: CtxSymbol => ^(sym.symbol)
  }

  def ^(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVXor(s.context.mkInt(symbol, s.getSort), s))

  def ^(s: Int): IntSymbol = new IntSymbol(symbol ^ s)

  def ~(): IntSymbol = new IntSymbol(~symbol)

  def neg: IntSymbol = this.~ + 1

  def >>(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => >>(sym.symbol)
    case sym: CtxSymbol => >>(sym.symbol)
  }

  def >>(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVAshr(s.context.mkInt(symbol, s.getSort), s))

  def >>(s: Int): IntSymbol = new IntSymbol(symbol >> s)

  def <<(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => <<(sym.symbol)
    case sym: CtxSymbol => <<(sym.symbol)
  }

  def <<(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVShl(s.context.mkInt(symbol, s.getSort), s))

  def <<(s: Int): IntSymbol = new IntSymbol(symbol << s)

  def eq(s: MySymbol): MySymbol = s match {
    case sym: CtxSymbol => eq(sym.symbol)
  }

  def eq(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkEq(s.context.mkInt(symbol, s.getSort), s))

  def eq(s: Int): Boolean = symbol == s

  def &&(s: MySymbol): MySymbol = s match {
    case sym: CtxSymbol => &&(sym.symbol)
  }

  def &&(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkAnd(s.context.mkInt(symbol, s.getSort), s))

  def ||(s: MySymbol): MySymbol = s match {
    case sym: CtxSymbol => ||(sym.symbol)
  }

  def ||(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkOr(s.context.mkInt(symbol, s.getSort), s))

  def ^^(s: MySymbol): MySymbol = s match {
    case sym: CtxSymbol => ^^(sym.symbol)
  }

  def ^^(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkXor(s.context.mkInt(symbol, s.getSort), s))

  def >=(s: MySymbol): CtxSymbol = s match {
    case sym: CtxSymbol => >=(sym.symbol)
  }

  def >=(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVSge(s.context.mkInt(symbol, s.getSort), s))

  def >=(s: Int): Boolean = symbol >= s

  def <(s: MySymbol): CtxSymbol = s match {
    case sym: CtxSymbol => <(sym.symbol)
  }

  def <(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVSlt(s.context.mkInt(symbol, s.getSort), s))

  def <(s: Int): Boolean = symbol < s

  def bitclr(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => bitclr(sym.symbol)
    case sym: CtxSymbol => bitclr(sym.symbol)
  }

  def bitclr(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVAnd(s.context.mkInt(symbol, s.getSort), s.context.mkNot(s.context.mkBVShl(s.context.mkInt(1, s.getSort), s))))

  def bitclr(s: Int): IntSymbol = &(~(1 << s))

  def bitset(s: MySymbol): MySymbol = s match {
    case sym: IntSymbol => bitset(sym.symbol)
    case sym: CtxSymbol => bitset(sym.symbol)
  }

  def bitset(s: Z3AST): CtxSymbol = new CtxSymbol(s.context.mkBVOr(s.context.mkInt(symbol, s.getSort), s.context.mkBVShl(s.context.mkInt(1, s.getSort), s)))

  def bitset(s: Int): IntSymbol = |(1 << s)

  override def toString(): String = symbol.toString
}