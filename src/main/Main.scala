package main

import java.io.File

import data.register._
import data.{DataSet, SymbolCounter}
import parser.ASTVisitor
import symbol.CtxSymbol
import z3.scala.Z3Context

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
object Main {

  val ctx = new Z3Context
  val state = new ArrayBuffer[State]
  val stack = new mutable.Stack[State]
  val queue = new mutable.Queue[State]
  var rom: ROM = null

  def main(args: Array[String]): Unit = {

    val file = new File("test_code") -> new File("asm")
    new ConvertToInputForm(file._1, file._2).convert()
    rom = new ASTVisitor().makeProgram(ctx, file._2)
    val init = first
    val initState = new State(state.size, init, null)
    state += initState
    stack.push(initState)
    //    queue += initState

    while (!stack.isEmpty) {
      val current = stack.pop
      //      val current = queue.dequeue
      val data = new DataSet(current)
      val dataArray = new Decoder().analyze(data)
      dataArray.foreach { d =>
        val s = new State(state.size, d, current)
        current.next += s
        println(s)
        println
        state += s
        if (!s.stop) stack.push(s)
        //        if (!s.stop) queue += s
      }
      //      current.path.path = null
      if (state.length >= 2) stack.clear
      //      if (state.length >= 500) queue.clear
    }
    new ResultWritter().write(new File("result.txt"))
  }

  def first: DataSet = {
    val make = new SymbolCounter(new mutable.HashMap[String, Int])
    val reg = new Register(new mutable.HashMap[Int, CtxSymbol], make)
    val mem = new Memory(Parameter.ioInit(make), make)
    val pc = new ProgramCounter(rom.getWord(0))
    val path = new PathCondition(null)
    val ccr = new ConditionRegister(make.makeSymbol(0, "c"))
    ccr.setI
    new DataSet(reg, mem, pc, ccr, path, make)
  }

  def simple(symbol: CtxSymbol): CtxSymbol = {
    val c = new CtxSymbol(ctx.simplifyAst(symbol.symbol))
    if (c.toString() == symbol.toString()) symbol
    else c
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

  def ioInit(s: SymbolCounter): mutable.HashMap[Int, CtxSymbol] = {
    val mem = new mutable.HashMap[Int, CtxSymbol]
    mem += 0xF700 -> new CtxSymbol(0,8) //TCR_0
    mem += 0xF701 -> new CtxSymbol(0x88,8) //TIORA_0
    mem += 0xF702 -> new CtxSymbol(0x88,8) //TIORC_0
    mem += 0xF703 -> new CtxSymbol(0xC0,8) //TSR_0
    mem += 0xF704 -> new CtxSymbol(0xE0,8) //TIER_0
    mem += 0xF705 -> new CtxSymbol(0xF8,8) //POCR_0
    mem += 0xF706 -> new CtxSymbol(0x00,8) //TCR_0
    mem += 0xF707 -> new CtxSymbol(0x00,8) //TCR_0
    mem += 0xF708 -> new CtxSymbol(0xFF,8) //GRA_0
    mem += 0xF709 -> new CtxSymbol(0xFF,8) //GRA_0
    mem += 0xF70A -> new CtxSymbol(0xFF,8) //GRB_0
    mem += 0xF70B -> new CtxSymbol(0xFF,8) //GRB_0
    mem += 0xF70C -> new CtxSymbol(0xFF,8) //GRC_0
    mem += 0xF70D -> new CtxSymbol(0xFF,8) //GRC_0
    mem += 0xF70E -> new CtxSymbol(0xFF,8) //GRD_0
    mem += 0xF70F -> new CtxSymbol(0xFF,8) //GRD_0
    mem += 0xF710 -> new CtxSymbol(0x00,8) //TCR_1
    mem += 0xF711 -> new CtxSymbol(0x88,8) //TIORA_1
    mem += 0xF712 -> new CtxSymbol(0x88,8) //TIORC_1
    mem += 0xF713 -> new CtxSymbol(0xC0,8) //TSR_1
    mem += 0xF714 -> new CtxSymbol(0xE0,8) //TIER_1
    mem += 0xF715 -> new CtxSymbol(0xF8,8) //POCR_1
    mem += 0xF716 -> new CtxSymbol(0x00,8) //TCR_1
    mem += 0xF717 -> new CtxSymbol(0x00,8) //TCR_1
    mem += 0xF718 -> new CtxSymbol(0xFF,8) //GRA_1
    mem += 0xF719 -> new CtxSymbol(0xFF,8) //GRA_1
    mem += 0xF71A -> new CtxSymbol(0xFF,8) //GRB_1
    mem += 0xF71B -> new CtxSymbol(0xFF,8) //GRB_1
    mem += 0xF71C -> new CtxSymbol(0xFF,8) //GRC_1
    mem += 0xF71D -> new CtxSymbol(0xFF,8) //GRC_1
    mem += 0xF71E -> new CtxSymbol(0xFF,8) //GRD_1
    mem += 0xF71F -> new CtxSymbol(0xFF,8) //GRD_1
    mem += 0xF720 -> new CtxSymbol(0xFC,8) //TSTR
    mem += 0xF721 -> new CtxSymbol(0x0E,8) //TMDR
    mem += 0xF722 -> new CtxSymbol(0x88,8) //TPMR
    mem += 0xF723 -> new CtxSymbol(0x80,8) //TFCR
    mem += 0xF724 -> new CtxSymbol(0xFF,8) //TOER
    mem += 0xF725 -> new CtxSymbol(0x00,8) //TOCR
    val rwkdr = s.makeSymbol(0xF72B, "m") & 0x87
    mem += 0xF72B -> rwkdr
    val rtccr1 = s.makeSymbol(0xF72C, "m") & 0xE0
    mem += 0xF72C -> rtccr1
    val rtccr2 = s.makeSymbol(0xF72D, "m") & 0x3F
    mem += 0xF72D -> rtccr2
    mem += 0xF72F -> new CtxSymbol(0x08,8) //RTCCSR
    mem += 0xF730 -> new CtxSymbol(0x40,8) //LVDCR
    mem += 0xF731 -> new CtxSymbol(0xFC,8) //LVDSR
    mem += 0xF740 -> new CtxSymbol(0x00,8) //SMR_2
    mem += 0xF741 -> new CtxSymbol(0xFF,8) //BRR_2
    mem += 0xF742 -> new CtxSymbol(0x00,8) //SCR3_2
    mem += 0xF743 -> new CtxSymbol(0xFF,8) //TDR_2
    mem += 0xF744 -> new CtxSymbol(0x84,8) //SSR_2
    mem += 0xF745 -> new CtxSymbol(0x00,8) //RDR_2
    mem += 0xF748 -> new CtxSymbol(0x00,8) //ICCR1
    mem += 0xF749 -> new CtxSymbol(0x7D,8) //ICCR2
    mem += 0xF74A -> new CtxSymbol(0x38,8) //ICMR
    mem += 0xF74B -> new CtxSymbol(0x00,8) //ICIER
    mem += 0xF74C -> new CtxSymbol(0x00,8) //ICSR
    mem += 0xF74D -> new CtxSymbol(0x00,8) //SAR
    mem += 0xF74E -> new CtxSymbol(0xFF,8) //ICDRT
    mem += 0xF74F -> new CtxSymbol(0xFF,8) //ICDRR
    mem += 0xF760 -> new CtxSymbol(0x78,8) //TMB1
    mem += 0xF761 -> new CtxSymbol(0x00,8) //TCB1
    mem += 0xFF90 -> new CtxSymbol(0x00,8) //FLMCR1
    mem += 0xFF91 -> new CtxSymbol(0x00,8) //FLMCR1
    mem += 0xFF92 -> new CtxSymbol(0x00,8) //FLPWCR
    mem += 0xFF93 -> new CtxSymbol(0x00,8) //EBR1
    mem += 0xFF9B -> new CtxSymbol(0x00,8) //FENR
    mem += 0xFFA0 -> new CtxSymbol(0x00,8) //TCRV0
    mem += 0xFFA1 -> new CtxSymbol(0x10,8) //TCSRV
    mem += 0xFFA2 -> new CtxSymbol(0xFF,8) //TCORA
    mem += 0xFFA3 -> new CtxSymbol(0xFF,8) //TCORB
    mem += 0xFFA4 -> new CtxSymbol(0x00,8) //TCNTV
    mem += 0xFFA5 -> new CtxSymbol(0xE2,8) //TCRV1
    mem += 0xFFA8 -> new CtxSymbol(0x00,8) //SMR
    mem += 0xFFA9 -> new CtxSymbol(0xFF,8) //BRR
    mem += 0xFFAA -> new CtxSymbol(0x00,8) //SCR3
    mem += 0xFFAB -> new CtxSymbol(0xFF,8) //TDR
    mem += 0xFFAC -> new CtxSymbol(0x84,8) //SSR
    mem += 0xFFAD -> new CtxSymbol(0x00,8) //RDR
    mem += 0xFFB0 -> new CtxSymbol(0x00,8) //ADDRA
    mem += 0xFFB1 -> new CtxSymbol(0x00,8) //ADDRA
    mem += 0xFFB2 -> new CtxSymbol(0x00,8) //ADDRB
    mem += 0xFFB3 -> new CtxSymbol(0x00,8) //ADDRB
    mem += 0xFFB4 -> new CtxSymbol(0x00,8) //ADDRC
    mem += 0xFFB5 -> new CtxSymbol(0x00,8) //ADDRC
    mem += 0xFFB6 -> new CtxSymbol(0x00,8) //ADDRC
    mem += 0xFFB7 -> new CtxSymbol(0x00,8) //ADDRC
    mem += 0xFFB8 -> new CtxSymbol(0x00,8) //ADCSR
    mem += 0xFFB9 -> new CtxSymbol(0x7E,8) //ADCR
    mem += 0xFFBC -> new CtxSymbol(0x00,8) //PWDEL
    mem += 0xFFBD -> new CtxSymbol(0xC0,8) //PWDEU
    mem += 0xFFBE -> new CtxSymbol(0xF7,8) //PWCR
    mem += 0xFFC0 -> new CtxSymbol(0xAA,8) //TCSRWD
    mem += 0xFFC1 -> new CtxSymbol(0x00,8) //TCWD
    mem += 0xFFC2 -> new CtxSymbol(0xFF,8) //TMWD
    mem += 0xFFC8 -> new CtxSymbol(0x80,8) //ABRKCR
    mem += 0xFFC9 -> new CtxSymbol(0x3F,8) //ABRKSR
    mem += 0xFFCA -> new CtxSymbol(0xFF,8) //BARH
    mem += 0xFFCB -> new CtxSymbol(0xFF,8) //BARL
    mem += 0xFFD0 -> new CtxSymbol(0x08,8) //PUCR1
    mem += 0xFFD1 -> new CtxSymbol(0x00,8) //PUCR5
    mem += 0xFFD4 -> new CtxSymbol(0x08,8) //PDR1
    mem += 0xFFD5 -> new CtxSymbol(0xE0,8) //PDR2
    mem += 0xFFD6 -> new CtxSymbol(0x00,8) //PDR3
    mem += 0xFFD8 -> new CtxSymbol(0x00,8) //PDR5
    mem += 0xFFD9 -> new CtxSymbol(0x00,8) //PDR6
    mem += 0xFFDA -> new CtxSymbol(0x88,8) //PDR7
    mem += 0xFFDB -> new CtxSymbol(0x1F,8) //PDR8
    mem += 0xFFE0 -> new CtxSymbol(0x00,8) //PMR1
    mem += 0xFFE1 -> new CtxSymbol(0x07,8) //PMR3
    mem += 0xFFE2 -> new CtxSymbol(0x00,8) //PMR5
    val pcr1 = s.makeSymbol(0xFFE4, "m") & 0x08
    mem += 0xFFE4 -> pcr1
    val pcr2 = s.makeSymbol(0xFFE5, "m") & 0xE0
    mem += 0xFFE5 -> pcr2
    mem += 0xFFE6 -> new CtxSymbol(0x00,8) //PCR3
    mem += 0xFFE8 -> new CtxSymbol(0x00,8) //PCR5
    mem += 0xFFE9 -> new CtxSymbol(0x00,8) //PCR6
    val pcr7 = s.makeSymbol(0xFFEA, "m") & 0x88
    mem += 0xFFEA -> pcr7
    val pcr8 = s.makeSymbol(0xFFEB, "m") & 0x1F
    mem += 0xFFEB -> pcr8
    mem += 0xFFF0 -> new CtxSymbol(0x00,8) //SYSCR1
    mem += 0xFFF1 -> new CtxSymbol(0x00,8) //SYSCR2
    mem += 0xFFF2 -> new CtxSymbol(0x70,8) //IEGR1
    mem += 0xFFF3 -> new CtxSymbol(0xC0,8) //IEGR2
    mem += 0xFFF4 -> new CtxSymbol(0x10,8) //IENR1
    mem += 0xFFF5 -> new CtxSymbol(0x1F,8) //IENR2
    mem += 0xFFF6 -> new CtxSymbol(0x30,8) //IRR1
    mem += 0xFFF7 -> new CtxSymbol(0x1F,8) //IRR2
    mem += 0xFFF8 -> new CtxSymbol(0xC0,8) //IWPR
    mem += 0xFFF9 -> new CtxSymbol(0x00,8) //MSTCR1
    mem += 0xFFFA -> new CtxSymbol(0x00,8) //MSTCR2
    mem += 0xFF09 -> new CtxSymbol(0x00,8) //スレーブアドレスレジスタ
    mem += 0xFF10 -> new CtxSymbol(0xFF,8) //EKR
    mem
  }

}