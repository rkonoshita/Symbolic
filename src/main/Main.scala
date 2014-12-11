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
  val state = new ArrayBuffer[State]
  val stack = new mutable.Queue[State]
  var rom: ROM = null

  def main(args: Array[String]): Unit = {

    val file = new File("target") -> new File("asm")
    new ConvertToInputForm(file._1, file._2).convert()
    rom = new ASTVisitor().makeProgram(ctx, file._2)
    val init = first
    val initState = new State(state.size, init, null)
    state += initState
    stack.+=(initState)

    while (!stack.isEmpty) {
      val current = stack.dequeue
      val data = new DataSet(current)
      val dataArray = new Decoder(ctx, rom).analyze(data)
      dataArray.foreach { d =>
        val s = new State(state.size, d, current)
        current.next += s
        println(s)
        println
        state += s
        if (!s.stop) stack += s
      }
      if (state.length >= 1000) stack.clear
    }
    new ResultWritter().write(new File("result.txt"))
  }

  def first(): DataSet = {
    println(rom.getWord(0))
    val reg = new Register(ctx, new mutable.HashMap[Int, MySymbol])
    val mem = new Memory(ctx, new mutable.HashMap[Int, MySymbol])
    val pc = new ProgramCounter(rom.getWord(0))
    val path = new PathCondition(ctx)
    val ccr = new ConditionRegister(ctx, new CtxSymbol(makeCCRSymbol))
    new DataSet(reg, mem, pc, ccr, path)
  }

  var rsym = -1

  def makeRegisterSymbol: Z3AST = {
    rsym += 1
    ctx.mkConst("r" + rsym, ctx.mkBVSort(32))
  }

  var msym = -1

  def makeMemorySymbol: Z3AST = {
    msym += 1
    ctx.mkConst("m" + msym, ctx.mkBVSort(8))
  }

  var csym = -1

  def makeCCRSymbol: Z3AST = {
    csym += 1
    ctx.mkConst("c" + csym, ctx.mkBVSort(8))
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

  def extract(range: Range, c: CtxSymbol): ArrayBuffer[Int] = extract(range, c.symbol)

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

  def ioInit(): mutable.HashMap[Int, MySymbol] = {
    val mem = new mutable.HashMap[Int,MySymbol]
    mem += 0xF700 -> new IntSymbol(0) //TCR_0
    mem += 0xF701 -> new IntSymbol(0x88) //TIORA_0
    mem += 0xF702 -> new IntSymbol(0x88) //TIORC_0
    mem += 0xF703 -> new IntSymbol(0xC0) //TSR_0
    mem += 0xF704 -> new IntSymbol(0xE0) //TIER_0
    mem += 0xF705 -> new IntSymbol(0xF8) //POCR_0
    mem += 0xF706 -> new IntSymbol(0) //TCR_0
    mem += 0xF707 -> new IntSymbol(0) //TCR_0
    mem += 0xF708 -> new IntSymbol(0xFF) //GRA_0
    mem += 0xF709 -> new IntSymbol(0xFF) //GRA_0
    mem += 0xF70A -> new IntSymbol(0xFF) //GRB_0
    mem += 0xF70B -> new IntSymbol(0xFF) //GRB_0
    mem += 0xF70C -> new IntSymbol(0xFF) //GRC_0
    mem += 0xF70D -> new IntSymbol(0xFF) //GRC_0
    mem += 0xF70E -> new IntSymbol(0xFF) //GRD_0
    mem += 0xF70F -> new IntSymbol(0xFF) //GRD_0
    mem += 0xF710 -> new IntSymbol(0) //TCR_1
    mem += 0xF711 -> new IntSymbol(0x88) //TIORA_1
    mem += 0xF712 -> new IntSymbol(0x88) //TIORC_1
    mem += 0xF713 -> new IntSymbol(0xC0) //TSR_1
    mem += 0xF714 -> new IntSymbol(0xE0) //TIER_1
    mem += 0xF715 -> new IntSymbol(0xF8) //POCR_1
    mem += 0xF716 -> new IntSymbol(0) //TCR_1
    mem += 0xF717 -> new IntSymbol(0) //TCR_1
    mem += 0xF718 -> new IntSymbol(0xFF) //GRA_1
    mem += 0xF719 -> new IntSymbol(0xFF) //GRA_1
    mem += 0xF71A -> new IntSymbol(0xFF) //GRB_1
    mem += 0xF71B -> new IntSymbol(0xFF) //GRB_1
    mem += 0xF71C -> new IntSymbol(0xFF) //GRC_1
    mem += 0xF71D -> new IntSymbol(0xFF) //GRC_1
    mem += 0xF71E -> new IntSymbol(0xFF) //GRD_1
    mem += 0xF71F -> new IntSymbol(0xFF) //GRD_1
    mem += 0xF720 -> new IntSymbol(0xFC) //TSTR
    mem += 0xF721 -> new IntSymbol(0x0E) //TMDR
    mem += 0xF722 -> new IntSymbol(0x88) //TPMR
    mem += 0xF723 -> new IntSymbol(0x80) //TFCR
    mem += 0xF724 -> new IntSymbol(0xFF) //TOER
    mem += 0xF725 -> new IntSymbol(0x00) //TOCR
    val rwkdr = new CtxSymbol(Main.makeMemorySymbol) & 0x87
    mem += 0xF72B -> rwkdr

  }

}