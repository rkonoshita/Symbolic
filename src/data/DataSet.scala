package data

import data.register._
import base.{Symbolic, State}
import symbol.CtxSymbol

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */

//寄せ集め
class DataSet(r: Register, m: Memory, p: ProgramCounter, c: ConditionRegister, pt: PathCondition, cs: Array[Boolean]) {

  val reg = new Register(r.reg)
  val mem = new Memory(m.mem, arrayClone(m.in), arrayClone(m.ib.clone()))
  val pc = new ProgramCounter(p.pc)
  val ccr = new ConditionRegister(c.getCcr)
  val path = new PathCondition(pt.path)
  val conset = cs

  override def clone(): DataSet = {
    new DataSet(
      new Register(reg.reg),
      new Memory(mem.mem, mem.in, mem.ib),
      new ProgramCounter(pc.pc),
      new ConditionRegister(ccr.getCcr),
      new PathCondition(path.path),
      conset.clone()
    )
  }

  def this(s: State, c: Array[Boolean]) = this(s.reg, s.mem, s.pc, s.ccr, s.path, c)

  def inputCheck(): Unit = {
    inputCheckIO1()
    inputCheckIO2()
    inputCheckIO3()
    inputCheckIO5()
    inputCheckIO6()
    inputCheckIO7()
    inputCheckIO8()
    inputCheckIOB()
  }

  //IOport1のチェック
  private def inputCheckIO1(): Unit = {
    if (mem.ib(0)) {
      val pmr1 = mem.getByte(0xFFE0) //0なら入出力ポート
      val pcr1 = mem.getByte(0xFFE4) //0なら入力ポート
      val pdr1 = mem.getByte(0xFFD4)
      val in = (pcr1 | pmr1).~() //1なら入力可能
      val data = (in.~() & pdr1) | (in & new CtxSymbol("m" + 0xFFD4 + "_" + mem.in(0), 8))
      mem.setByte(data, 0xFFD4)
      mem.in(0) = mem.in(0) + 1
      mem.ib(0) = false
    }
  }

  //IOport2のチェック
  private def inputCheckIO2(): Unit = {
    if (mem.ib(1)) {
      val pcr2 = mem.getByte(0xFFE5) | 0xE0 //0なら入力ポート
      val pdr2 = mem.getByte(0xFFD5)
      val data = (pdr2 & pcr2) | (pcr2.~() & new CtxSymbol("m" + 0xFFD5 + "_" + mem.in(1), 8))
      mem.setByte(data, 0xFFD5)
      mem.in(1) = mem.in(1) + 1
      mem.ib(1) = false
    }
  }

  //IOport3のチェック
  private def inputCheckIO3(): Unit = {
    if (mem.ib(2)) {
      val pcr3 = mem.getByte(0xFFE6) //0なら入力ポート
      val pdr3 = mem.getByte(0xFFD6)
      val data = (pdr3 & pcr3) | (pcr3.~() & new CtxSymbol("m" + 0xFFD6 + "_" + mem.in(2), 8))
      mem.setByte(data, 0xFFD6)
      mem.in(2) = mem.in(2) + 1
      mem.ib(2) = false
    }
  }

  //IOport5のチェック
  private def inputCheckIO5(): Unit = {
    if (mem.ib(3)) {
      val pmr5 = mem.getByte(0xFFE2) //0なら入出力ポート
      val pcr5 = mem.getByte(0xFFE8) //0なら入力ポート
      val pdr5 = mem.getByte(0xFFD8)
      val in = (pcr5 | pmr5).~() //1なら入力可能
      val data = (in.~() & pdr5) | (in & new CtxSymbol("m" + 0xFFD8 + "_" + mem.in(3), 8))
      mem.setByte(data, 0xFFD8)
      mem.in(3) = mem.in(3) + 1
      mem.ib(3) = false
    }
  }

  //IOport6のチェック
  private def inputCheckIO6(): Unit = {
    if (mem.ib(4)) {
      val pcr6 = mem.getByte(0xFFE9) //0なら入力ポート
      val pdr6 = mem.getByte(0xFFD9)
      val data = (pdr6 & pcr6) | (pcr6.~() & new CtxSymbol("m" + 0xFFD9 + "_" + mem.in(4), 8))
      mem.setByte(data, 0xFFD9)
      mem.in(4) = mem.in(4) + 1
      mem.ib(4) = false
    }
  }

  //IOport7のチェック
  private def inputCheckIO7(): Unit = {
    if (mem.ib(5)) {
      val pcr7 = mem.getByte(0xFFEA) | 0x88 //0なら入力ポート
      val pdr7 = mem.getByte(0xFFDA)
      val data = (pdr7 & pcr7) | (pcr7.~() & new CtxSymbol("m" + 0xFFDA + "_" + mem.in(5), 8))
      mem.setByte(data, 0xFFDA)
      mem.in(5) = mem.in(5) + 1
      mem.ib(5) = false
    }
  }

  //IOport8のチェック
  private def inputCheckIO8(): Unit = {
    if (mem.ib(6)) {
      val pcrb = mem.getByte(0xFFEB) | 0x1F //0なら入力ポート
      val pdrb = mem.getByte(0xFFDB)
      val data = (pdrb & pcrb) | (pcrb.~() & new CtxSymbol("m" + 0xFFDB + "_" + mem.in(6), 8))
      mem.setByte(data, 0xFFDB)
      mem.in(6) = mem.in(6) + 1
      mem.ib(6) = false
    }
  }

  //IOportBのチェック
  private def inputCheckIOB(): Unit = {
    if (mem.ib(7)) {
      val data = new CtxSymbol("m" + 0xFFDD + "_" + mem.in(7), 8)
      mem.setByte(data, 0xFFDD)
      mem.in(7) = mem.in(7) + 1
      mem.ib(7) = false
    }
  }

  def arrayClone(array: Array[Int]): Array[Int] = {
    val cp = new Array[Int](array.length)
    for (i <- 0 until array.length) cp(i) = array(i)
    cp
  }

  def arrayClone(array: Array[Boolean]): Array[Boolean] = {
    val cp = new Array[Boolean](array.length)
    for (i <- 0 until array.length) cp(i) = array(i)
    cp
  }

}

//0:stop flag
//1:divop flag