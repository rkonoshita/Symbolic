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
      val dataArray = new Decoder().analyze(data)
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
    val mem = new Memory(ctx, Parameter.ioInit)
    val pc = new ProgramCounter(rom.getWord(0))
    val path = new PathCondition(ctx)
    val ccr = new ConditionRegister(ctx, new CtxSymbol(makeCCRSymbol))
    ccr.setI
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
    val mem = new mutable.HashMap[Int, MySymbol]
    mem += 0xF700 -> new IntSymbol(0) //TCR_0
    mem += 0xF701 -> new IntSymbol(0x88) //TIORA_0
    mem += 0xF702 -> new IntSymbol(0x88) //TIORC_0
    mem += 0xF703 -> new IntSymbol(0xC0) //TSR_0
    mem += 0xF704 -> new IntSymbol(0xE0) //TIER_0
    mem += 0xF705 -> new IntSymbol(0xF8) //POCR_0
    mem += 0xF706 -> new IntSymbol(0x00) //TCR_0
    mem += 0xF707 -> new IntSymbol(0x00) //TCR_0
    mem += 0xF708 -> new IntSymbol(0xFF) //GRA_0
    mem += 0xF709 -> new IntSymbol(0xFF) //GRA_0
    mem += 0xF70A -> new IntSymbol(0xFF) //GRB_0
    mem += 0xF70B -> new IntSymbol(0xFF) //GRB_0
    mem += 0xF70C -> new IntSymbol(0xFF) //GRC_0
    mem += 0xF70D -> new IntSymbol(0xFF) //GRC_0
    mem += 0xF70E -> new IntSymbol(0xFF) //GRD_0
    mem += 0xF70F -> new IntSymbol(0xFF) //GRD_0
    mem += 0xF710 -> new IntSymbol(0x00) //TCR_1
    mem += 0xF711 -> new IntSymbol(0x88) //TIORA_1
    mem += 0xF712 -> new IntSymbol(0x88) //TIORC_1
    mem += 0xF713 -> new IntSymbol(0xC0) //TSR_1
    mem += 0xF714 -> new IntSymbol(0xE0) //TIER_1
    mem += 0xF715 -> new IntSymbol(0xF8) //POCR_1
    mem += 0xF716 -> new IntSymbol(0x00) //TCR_1
    mem += 0xF717 -> new IntSymbol(0x00) //TCR_1
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
    val rtccr1 = new CtxSymbol(Main.makeMemorySymbol) & 0xE0
    mem += 0xF72C -> rtccr1
    val rtccr2 = new CtxSymbol(Main.makeMemorySymbol) & 0x3F
    mem += 0xF72D -> rtccr2
    mem += 0xF72F -> new IntSymbol(0x08) //RTCCSR
    mem += 0xF730 -> new IntSymbol(0x40) //LVDCR
    mem += 0xF731 -> new IntSymbol(0xFC) //LVDSR
    mem += 0xF740 -> new IntSymbol(0x00) //SMR_2
    mem += 0xF741 -> new IntSymbol(0xFF) //BRR_2
    mem += 0xF742 -> new IntSymbol(0x00) //SCR3_2
    mem += 0xF743 -> new IntSymbol(0xFF) //TDR_2
    mem += 0xF744 -> new IntSymbol(0x84) //SSR_2
    mem += 0xF745 -> new IntSymbol(0x00) //RDR_2
    mem += 0xF748 -> new IntSymbol(0x00) //ICCR1
    mem += 0xF749 -> new IntSymbol(0x7D) //ICCR2
    mem += 0xF74A -> new IntSymbol(0x38) //ICMR
    mem += 0xF74B -> new IntSymbol(0x00) //ICIER
    mem += 0xF74C -> new IntSymbol(0x00) //ICSR
    mem += 0xF74D -> new IntSymbol(0x00) //SAR
    mem += 0xF74E -> new IntSymbol(0xFF) //ICDRT
    mem += 0xF74F -> new IntSymbol(0xFF) //ICDRR
    mem += 0xF760 -> new IntSymbol(0x78) //TMB1
    mem += 0xF761 -> new IntSymbol(0x00) //TCB1
    mem += 0xFF90 -> new IntSymbol(0x00) //FLMCR1
    mem += 0xFF91 -> new IntSymbol(0x00) //FLMCR1
    mem += 0xFF92 -> new IntSymbol(0x00) //FLPWCR
    mem += 0xFF93 -> new IntSymbol(0x00) //EBR1
    mem += 0xFF9B -> new IntSymbol(0x00) //FENR
    mem += 0xFFA0 -> new IntSymbol(0x00) //TCRV0
    mem += 0xFFA1 -> new IntSymbol(0x10) //TCSRV
    mem += 0xFFA2 -> new IntSymbol(0xFF) //TCORA
    mem += 0xFFA3 -> new IntSymbol(0xFF) //TCORB
    mem += 0xFFA4 -> new IntSymbol(0x00) //TCNTV
    mem += 0xFFA5 -> new IntSymbol(0xE2) //TCRV1
    mem += 0xFFA8 -> new IntSymbol(0x00) //SMR
    mem += 0xFFA9 -> new IntSymbol(0xFF) //BRR
    mem += 0xFFAA -> new IntSymbol(0x00) //SCR3
    mem += 0xFFAB -> new IntSymbol(0xFF) //TDR
    mem += 0xFFAC -> new IntSymbol(0x84) //SSR
    mem += 0xFFAD -> new IntSymbol(0x00) //RDR
    mem += 0xFFB0 -> new IntSymbol(0x00) //ADDRA
    mem += 0xFFB1 -> new IntSymbol(0x00) //ADDRA
    mem += 0xFFB2 -> new IntSymbol(0x00) //ADDRB
    mem += 0xFFB3 -> new IntSymbol(0x00) //ADDRB
    mem += 0xFFB4 -> new IntSymbol(0x00) //ADDRC
    mem += 0xFFB5 -> new IntSymbol(0x00) //ADDRC
    mem += 0xFFB6 -> new IntSymbol(0x00) //ADDRC
    mem += 0xFFB7 -> new IntSymbol(0x00) //ADDRC
    mem += 0xFFB8 -> new IntSymbol(0x00) //ADCSR
    mem += 0xFFB9 -> new IntSymbol(0x7E) //ADCR
    mem += 0xFFBC -> new IntSymbol(0x00) //PWDEL
    mem += 0xFFBD -> new IntSymbol(0xC0) //PWDEU
    mem += 0xFFBE -> new IntSymbol(0xF7) //PWCR
    mem += 0xFFC0 -> new IntSymbol(0xAA) //TCSRWD
    mem += 0xFFC1 -> new IntSymbol(0x00) //TCWD
    mem += 0xFFC2 -> new IntSymbol(0xFF) //TMWD
    mem += 0xFFC8 -> new IntSymbol(0x80) //ABRKCR
    mem += 0xFFC9 -> new IntSymbol(0x3F) //ABRKSR
    mem += 0xFFCA -> new IntSymbol(0xFF) //BARH
    mem += 0xFFCB -> new IntSymbol(0xFF) //BARL
    mem += 0xFFD0 -> new IntSymbol(0x08) //PUCR1
    mem += 0xFFD1 -> new IntSymbol(0x00) //PUCR5
    mem += 0xFFD4 -> new IntSymbol(0x08) //PDR1
    mem += 0xFFD5 -> new IntSymbol(0xE0) //PDR2
    mem += 0xFFD6 -> new IntSymbol(0x00) //PDR3
    mem += 0xFFD8 -> new IntSymbol(0x00) //PDR5
    mem += 0xFFD9 -> new IntSymbol(0x00) //PDR6
    mem += 0xFFDA -> new IntSymbol(0x88) //PDR7
    mem += 0xFFDB -> new IntSymbol(0x1F) //PDR8
    mem += 0xFFE0 -> new IntSymbol(0x00) //PMR1
    mem += 0xFFE1 -> new IntSymbol(0x00) //PMR5 ↓と逆かも？
    mem += 0xFFE2 -> new IntSymbol(0x07) //PMR3
    val pcr1 = new CtxSymbol(Main.makeMemorySymbol) & 0x08
    mem += 0xFFE4 -> pcr1
    val pcr2 = new CtxSymbol(Main.makeMemorySymbol) & 0xE0
    mem += 0xFFE5 -> pcr2
    mem += 0xFFE6 -> new IntSymbol(0x00) //PCR3
    mem += 0xFFE8 -> new IntSymbol(0x00) //PCR5
    mem += 0xFFE9 -> new IntSymbol(0x00) //PCR6
    val pcr7 = new CtxSymbol(Main.makeMemorySymbol) & 0x88
    mem += 0xFFEA -> pcr7
    val pcr8 = new CtxSymbol(Main.makeMemorySymbol) & 0x1F
    mem += 0xFFEB -> pcr8
    mem += 0xFFF0 -> new IntSymbol(0x00) //SYSCR1
    mem += 0xFFF1 -> new IntSymbol(0x00) //SYSCR2
    mem += 0xFFF2 -> new IntSymbol(0x70) //IEGR1
    mem += 0xFFF3 -> new IntSymbol(0xC0) //IEGR2
    mem += 0xFFF4 -> new IntSymbol(0x10) //IENR1
    mem += 0xFFF5 -> new IntSymbol(0x1F) //IENR2
    mem += 0xFFF6 -> new IntSymbol(0x30) //IRR1
    mem += 0xFFF7 -> new IntSymbol(0x1F) //IRR2
    mem += 0xFFF8 -> new IntSymbol(0xC0) //IWPR
    mem += 0xFFF9 -> new IntSymbol(0x00) //MSTCR1
    mem += 0xFFFA -> new IntSymbol(0x00) //MSTCR2
    mem += 0xFF09 -> new IntSymbol(0x00) //スレーブアドレスレジスタ
    mem += 0xFF10 -> new IntSymbol(0xFF) //EKR
    mem
  }

}