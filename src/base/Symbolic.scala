package base

import java.io.File

import data.DataSet
import data.register._
import parser.ASTVisitor
import symbol.CtxSymbol
import z3.scala.Z3Context

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
object Symbolic {

  val ctx = new Z3Context
  val sol = ctx.mkSolver
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

    val t = time {
      while (!stack.isEmpty) {
        val current = stack.pop
        //val current = queue.dequeue
        val data = new DataSet(current)
        //ここらへんにセンサ関係書きたい
        input(data)

        val dataArray = new Decoder().analyze(data)
        dataArray.foreach { d =>
          val s = new State(state.size, d, current)
          current.next += s
          println(s)
          println
          state += s
          if (s.reach) {
            if (!s.stop) stack.push(s)
            //queue += s
            val error = s.stackError
            if (error._1) {
              stack.clear
              println(error._2.get)
            }
          }
        }
        if (state.length >= 40000) stack.clear
        //if (state.length >= 500) queue.clear
      }
    }
    new ResultWritter().write(new File("result.txt"), t)
  }

  def first: DataSet = {
    val reg = new Register(new CtxSymbol(ctx.mkConst("reg", ctx.mkArraySort(ctx.mkBVSort(4), ctx.mkBVSort(32)))))
    val mem = Parameter.ioInit
    val pc = new ProgramCounter(rom.getWord(0))
    val path = new PathCondition(null)
    val ccr = new ConditionRegister(new CtxSymbol(ctx.mkConst("ccr", ctx.mkBVSort(8))))
    ccr.setI
    new DataSet(reg, mem, pc, ccr, path)
  }

  def simple(symbol: CtxSymbol): CtxSymbol = {
    val c = new CtxSymbol(ctx.simplifyAst(symbol.symbol))
    if (c.toString() == symbol.toString()) symbol
    else c
  }

  var input = 0

  def input(data: DataSet): Unit = {
    val pcr3 = data.mem.getByte(0xFFE6)
    val pdr3 = data.mem.getByte(0xFFD6)
    val data3 = pdr3 & pcr3 | ctx.mkConst("m" + 0xFFDB + "_" + input, ctx.mkBVSort(8))
    data.mem.setByte(data3, 0xFFD6)
    input += 1
  }

  def time(proc: => Unit): Long = {
    val longtime = System.currentTimeMillis
    proc
    System.currentTimeMillis - longtime
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

  def ioInit(): Memory = {
    val ctx = Symbolic.ctx
    val mem = new Memory(new CtxSymbol(ctx.mkConst("mem", ctx.mkArraySort(ctx.mkBVSort(16), ctx.mkBVSort(8)))))
    mem.setByte(0x00, 0xF700) //TCR_0
    mem.setByte(0x88, 0xF701) //TIORA_0
    mem.setByte(0x88, 0xF702) //TIORC_0
    mem.setByte(0xC0, 0xF703) //TSR_0
    mem.setByte(0xE0, 0xF704) //TIER_0
    mem.setByte(0xF8, 0xF705) //POCR_0
    mem.setWord(0x0000, 0xF706) //TCR_0
    mem.setWord(0xFFFF, 0xF708) //GRA_0
    mem.setWord(0xFFFF, 0xF70A) //GRB_0
    mem.setWord(0xFFFF, 0xF70C) //GRC_0
    mem.setWord(0xFFFF, 0xF70E) //GRD_0
    mem.setByte(0x00, 0xF710) //TCR_1
    mem.setByte(0x88, 0xF711) //TIORA_1
    mem.setByte(0x88, 0xF712) //TIORC_1
    mem.setByte(0xC0, 0xF713) //TSR_1
    mem.setByte(0xE0, 0xF714) //TIER_1
    mem.setByte(0xF8, 0xF715) //POCR_1
    mem.setWord(0x0000, 0xF716) //TCR_1
    mem.setWord(0xFFFF, 0xF718) //GRA_1
    mem.setWord(0xFFFF, 0xF71A) //GRB_1
    mem.setWord(0xFFFF, 0xF71C) //GRC_1
    mem.setWord(0xFFFF, 0xF71E) //GRD_1
    mem.setByte(0xFC, 0xF720) //TSTR
    mem.setByte(0x0E, 0xF721) //TMDR
    mem.setByte(0x88, 0xF722) //TPMR
    mem.setByte(0x80, 0xF723) //TFCR
    mem.setByte(0xFF, 0xF724) //TOER
    mem.setByte(0x00, 0xF725) //TOCR
    mem.setByte(mem.getByte(0xF72B) & 0x87, 0xF72B) //RWKDR
    mem.setByte(mem.getByte(0xF72C) & 0xE0, 0xF72C) //RTCCR1
    mem.setByte(mem.getByte(0xF72D) & 0x3F, 0xF72D) //RTCCR2
    mem.setByte(0x08, 0xF72F) //RTCCSR
    mem.setByte(0x40, 0xF730) //LVDCR
    mem.setByte(0xFC, 0xF731) //LVDSR
    mem.setByte(0x00, 0xF740) //SMR_2
    mem.setByte(0xFF, 0xF741) //BRR_2
    mem.setByte(0x00, 0xF742) //SCR3_2
    mem.setByte(0xFF, 0xF743) //TDR_2
    mem.setByte(0x84, 0xF744) //SSR_2
    mem.setByte(0x00, 0xF745) //RDR_2
    mem.setByte(0x00, 0xF748) //ICCR1
    mem.setByte(0x7D, 0xF749) //ICCR2
    mem.setByte(0x38, 0xF74A) //ICMR
    mem.setByte(0x00, 0xF74B) //ICIER
    mem.setByte(0x00, 0xF74C) //ICSR
    mem.setByte(0x00, 0xF74D) //SAR
    mem.setByte(0xFF, 0xF74E) //ICDRT
    mem.setByte(0xFF, 0xF74F) //ICDRR
    mem.setByte(0x78, 0xF760) //TMB1
    mem.setByte(0x00, 0xF761) //TCB1
    mem.setWord(0x0000, 0xFF90) //FLMCR1
    mem.setByte(0x00, 0xFF92) //FLPWCR
    mem.setByte(0x00, 0xFF93) //EBR1
    mem.setByte(0x00, 0xFF9B) //FENR
    mem.setByte(0x00, 0xFFA0) //TCRV0
    mem.setByte(0x10, 0xFFA1) //TCSRV
    mem.setByte(0xFF, 0xFFA2) //TCORA
    mem.setByte(0xFF, 0xFFA3) //TCORB
    mem.setByte(0x00, 0xFFA4) //TCNTV
    mem.setByte(0xE2, 0xFFA5) //TCRV1
    mem.setByte(0x00, 0xFFA8) //SMR
    mem.setByte(0xFF, 0xFFA9) //BRR
    mem.setByte(0x00, 0xFFAA) //SCR3
    mem.setByte(0xFF, 0xFFAB) //TDR
    mem.setByte(0x84, 0xFFAC) //SSR
    mem.setByte(0x00, 0xFFAD) //RDR
    mem.setWord(0x0000, 0xFFB0) //ADDRA
    mem.setWord(0x0000, 0xFFB2) //ADDRB
    mem.setWord(0x0000, 0xFFB4) //ADDRC
    mem.setWord(0x0000, 0xFFB6) //ADDRD
    mem.setByte(0x00, 0xFFB8) //ADCSR
    mem.setByte(0x7E, 0xFFB9) //ADCR
    mem.setByte(0x00, 0xFFBC) //PWDEL
    mem.setByte(0xC0, 0xFFBD) //PWDEU
    mem.setByte(0xF7, 0xFFBE) //PWCR
    mem.setByte(0xAA, 0xFFC0) //TCSRWD
    mem.setByte(0x00, 0xFFC1) //TCWD
    mem.setByte(0xFF, 0xFFC2) //TMWD
    mem.setByte(0x80, 0xFFC8) //ABRKCR
    mem.setByte(0x3F, 0xFFC9) //ABRKSR
    mem.setByte(0xFF, 0xFFCA) //BARH
    mem.setByte(0xFF, 0xFFCB) //BARL
    mem.setByte(0x08, 0xFFD0) //PUCR1
    mem.setByte(0x00, 0xFFD1) //PUCR5
    mem.setByte(0x08, 0xFFD4) //PDR1
    mem.setByte(0xE0, 0xFFD5) //PDR2
    mem.setByte(0x00, 0xFFD6) //PDR3
    mem.setByte(0x00, 0xFFD8) //PDR5
    mem.setByte(0x00, 0xFFD9) //PDR6
    mem.setByte(0x88, 0xFFDA) //PDR7
    mem.setByte(0x1F, 0xFFDB) //PDR8
    mem.setByte(0x00, 0xFFE0) //PMR1
    mem.setByte(0x07, 0xFFE1) //PMR3
    mem.setByte(0x00, 0xFFE2) //PMR5
    mem.setByte(mem.getByte(0xFFE4) & 0x08, 0xFFE4) //PCR1
    mem.setByte(mem.getByte(0xFFE5) & 0xE0, 0xFFE5) //PCR2
    mem.setByte(0x00, 0xFFE6) //PCR3
    mem.setByte(0x00, 0xFFE8) //PCR5
    mem.setByte(0x00, 0xFFE9) //PCR6
    mem.setByte(mem.getByte(0xFFEA) & 0x88, 0xFFEA) //PCR7
    mem.setByte(mem.getByte(0xFFEB) & 0x1F, 0xFFEB) //PCR8
    mem.setByte(0x00, 0xFFF0) //SYSCR1
    mem.setByte(0x00, 0xFFF1) //SYSCR2
    mem.setByte(0x70, 0xFFF2) //IEGR1
    mem.setByte(0xC0, 0xFFF3) //IEGR2
    mem.setByte(0x10, 0xFFF4) //IENR1
    mem.setByte(0x1F, 0xFFF5) //IENR2
    mem.setByte(0x30, 0xFFF6) //IRR1
    mem.setByte(0x1F, 0xFFF7) //IRR2
    mem.setByte(0xC0, 0xFFF8) //IWPR
    mem.setByte(0x00, 0xFFF9) //MSTCR1
    mem.setByte(0x00, 0xFFFA) //MSTCR2
    mem.setByte(0x00, 0xFF09) //スレーブアドレスレジスタ
    mem.setByte(0xFF, 0xFF10) //EKR
    mem
  }

}