package main

import data.register.Register
import forZ3.{ExprVisitor, ParserForZ3}
import symbol.NewSymbol
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/12.
 */
object Main {

  val ctx = new Z3Context
  var symnum = -1

  def main(args: Array[String]): Unit = {

    val hash = new mutable.HashMap[Int, Z3AST]
    for (i <- 0 until 8) hash += i -> makeSymbol(32)
    val reg = new Register(ctx, hash)

    val s = ctx.mkSolver
    val reg7 = reg.reg(7)
    reg.setByte(reg.getByte(0xF), 0xF)
    val a = ctx.mkEq(reg.reg(7), reg7)
    println(reg.reg(7))
    println
//    s.assertCnstr(a)
    reg.setByte(reg.getByte(7), 7)
    val b = ctx.mkEq(reg.reg(7), reg7)
    println(reg.reg(7))
    println
//    s.assertCnstr(b)
    reg.setWord(reg.getWord(7), 7)
    val c = ctx.mkEq(reg.reg(7), reg7)
    println(reg.reg(7))
    println
//    s.assertCnstr(c)
    reg.setWord(reg.getWord(0xF), 0xF)
    val d = ctx.mkEq(reg.reg(7), reg7)
    println(reg.reg(7))
    s.assertCnstr(d)

    println(s.check())
  }

  def makeSymbol(size: Int): Z3AST = {
    symnum += 1
    ctx.mkConst("s" + symnum, ctx.mkBVSort(size))
  }

}
