package main

import java.io.File

import data.DataSet
import data.register._
import parser.ASTVisitor
import symbol.{CtxSymbol, IntSymbol, MySymbol}
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
object Main {

  val ctx = new Z3Context
  var symnum = -1
  val state = new ArrayBuffer[State]
  val stack = new mutable.Stack[State]

  def main(args: Array[String]): Unit = {
    val file = new File("target") -> new File("asm")
    new ConvertToInputForm(file._1, file._2).convert()
    val memory = new ASTVisitor().makeProgram(ctx, file._2)
    val init = first(memory)
    val initState = new State(state.size, init, null)
    state += initState
    stack.push(initState)

    while (!stack.isEmpty) {
      val current = stack.pop
      val data = new DataSet(current)
      val dataArray = new Decoder(ctx).analyze(data)
      dataArray.foreach { d =>
        val s = new State(state.size, d, current)
        current.next += s
        println(s)
        println
        state += s
        if (!s.stop) stack.push(s)
      }
      if (state.length >= 5000) stack.clear
    }
    new ResultWritter().write(new File("result.txt"))
  }

  def first(mem: Memory): DataSet = {
    val reg = new Register(ctx, new mutable.HashMap[Int, MySymbol])
    val pc = new ProgramCounter(mem.getWord(0).asInstanceOf[IntSymbol].symbol)
    val path = new PathCondition(ctx)
    val ccr = new ConditionRegister(ctx, new CtxSymbol(makeSymbol))
    new DataSet(reg, mem, pc, ccr, path)
  }

  def makeSymbol: Z3AST = {
    symnum += 1
    ctx.mkConst("s" + symnum, ctx.mkBVSort(32))
  }

  def extract(range: Range, ast: Z3AST): ArrayBuffer[Int] = {
    val s = ctx.mkSolver
    val buf = new ArrayBuffer[Int]
    range.foreach { n =>
      s.push
      s.assertCnstr(ctx.mkEq(ast, ctx.mkInt(n, ast.getSort)))
      if (s.check.get) buf += n
      s.pop(1)
    }
    buf
  }

}

object Parameter {

  val start = new mutable.HashMap[String, Int]
  start += "V" -> 0
  start += "P" -> 0x0100
  start += "C" -> 0x0100
  start += "D" -> 0x0100
  start += "B" -> 0xE800
  start += "R" -> 0xE800

  val size = new mutable.HashMap[String, Int]

  def sizeset(map: mutable.HashMap[String, Int]): Unit = size ++= map

  def getStart: mutable.HashMap[String, Int] = {
    val s = new mutable.HashMap[String, Int]
    start.foreach(v => s.put(v._1, v._2))
    s
  }

}