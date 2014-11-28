package main

import data.DataSet
import symbol.{MySymbol, CtxSymbol, IntSymbol}
import z3.scala.Z3Context

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/17.
 */
class Decoder(c: Z3Context) {

  private val ctx = c

  def analyze(data: DataSet): Unit = {
    decode(data.clone, data.pc.pc)
  }

  def decode(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    data.mem.getByte(pc) match {
      case op0: IntSymbol => decodeInt(data, pc)
      //      case op0: CtxSymbol => decodeSymbol(data, pc)
    }
  }

  def decodeInt(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = ArrayBuffer[DataSet](data)
    val op0 = data.mem.getByte(pc).asInstanceOf[IntSymbol].symbol
    op0 & 0xF0 match {
      case 0x50 =>
        op0 match {
          case 0x54 =>
            //RTS [54][70]
            val sp = data.reg.getLong(7).asInstanceOf[IntSymbol].symbol //スタックポインタであるER7は必ず整数と決め打ち！
            data.pc.setPc(data.mem.getWord(sp).asInstanceOf[IntSymbol].symbol) //Intである退避PCなので、整数と決め打ち！
            data.reg.setLong(new IntSymbol(sp + 2), 7)
          case 0x5E =>
            //JSR Abs:24 [5E][Abs][Abs][Abs]
            val abs = data.mem.getLong(pc).asInstanceOf[IntSymbol].symbol & 0x00FFFFFF
            val sp = data.reg.getLong(7).asInstanceOf[IntSymbol].symbol
            val stock = data.pc.pc
            data.mem.setWord(new IntSymbol(stock), sp - 2)
            data.reg.setLong(new IntSymbol(sp - 2), 7)
            data.pc.setPc(abs)
        }
      case 0x60 =>
        op0 match {
          case 0x6A =>
            val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
            op1 & 0xF0 match {
              case 0x80 =>
                //MOV.B Reg,Abs [6A][8reg][absh][absl]
                val reg = data.reg.getByte(op1)
                val absh = (data.mem.getByte(pc + 2).asInstanceOf[IntSymbol].symbol & 0xFF) << 8
                val abs = (data.mem.getByte(pc + 2).asInstanceOf[IntSymbol].symbol & 0xFF) | absh
                data.mem.setByte(reg, abs)
                data.ccr.ccr = data.ccr.clearV
                buf = checkZ(reg, buf, 8)
                buf = checkN(reg, buf, 8)
            }
          case 0x6D =>
            val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
            op1 & 0xF0 match {
              case 0xF0 =>
                //PUSH.W Reg [6D][FReg]
                val reg = data.reg.getWord(op1)
                val sp = data.reg.getLong(7).asInstanceOf[IntSymbol].symbol //スタックポインタであるER7は必ず整数と決め打ち！
                data.mem.setWord(reg, sp - 2)
                data.reg.setLong(new IntSymbol(sp - 2), 7)
                data.ccr.ccr = data.ccr.clearV
                buf = checkZ(reg, buf, 16)
                buf = checkN(reg, buf, 16)
            }
        }
      case 0x70 =>
        op0 match {
          case 0x7F =>
            val op2 = data.mem.getByte(pc + 2).asInstanceOf[IntSymbol].symbol
            op2 match {
              case 0x70 =>
                //BSET.B Imm,Abs [7F][Abs][70][Imm0]
                val abs = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
                val imm = data.mem.getByte(pc + 3).asInstanceOf[IntSymbol].symbol & 0x07
                val bset = data.mem.getByte(abs).bitset(imm)
                data.mem.setByte(bset, abs)
            }
        }
      case 0x80 =>
        //ADD.B Imm,Reg [8Reg][Imm]
        val imm = data.mem.getByte(pc + 1)
        val reg = data.reg.getByte(op0)
        val add = reg + imm
        data.reg.setByte(add, op0)
        buf = checkC(reg, imm, add, buf, 8)
        buf = checkV(reg, imm, add, buf, 8)
        buf = checkZ(add, buf, 8)
        buf = checkN(add, buf, 8)
        buf = checkH(reg, imm, add, buf, 8)
      case 0xE0 =>
        //AND.B Imm,Reg [Ereg][Imm]
        val imm = data.mem.getByte(pc + 1)
        val and = data.reg.getByte(op0) & imm
        data.reg.setByte(and, op0)
        data.ccr.ccr = data.ccr.clearV
        buf = checkZ(imm, buf, 8)
        buf = checkN(imm, buf, 8)
      case 0xF0 =>
        //MOV.B Imm,Reg [Freg][Imm]
        val imm = data.mem.getByte(pc + 1)
        data.reg.setByte(imm, op0)
        data.ccr.ccr = data.ccr.clearV
        buf = checkZ(imm, buf, 8)
        buf = checkN(imm, buf, 8)
    }
    buf
  }

  def checkC(data1: MySymbol, data2: MySymbol, res: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    val and =
      size match {
        case 8 => 0x80
        case 16 => 0x8000
        case 32 => 0x80000000
      }
    buf.foreach { b =>
      (data1, data2, res) match {
        case (d1: IntSymbol, d2: IntSymbol, r: IntSymbol) =>
          val bc = b.clone
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and)
          val bool2 = ((d1 & and).eq(and) || (d2 & and).eq(and)) && (r & and).eq(and)
          bc.ccr.ccr = (if (bool1 || bool2) bc.ccr.setC else bc.ccr.clearC)
          ans += bc
        case _ =>
          val s = ctx.mkSolver
          s.push
          val bool1 = (data1 & and).eq(new IntSymbol(and)) && (data2 & and).eq(new IntSymbol(and))
          val bool2 = ((data1 & and).eq(new IntSymbol(and)) || (data2 & and).eq(new IntSymbol(and))) && ((res & and).eq(new IntSymbol(and)))
          val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
          s.assertCnstr(bool.symbol)
          val b1 = s.check.get
          s.pop(1)
          s.assertCnstr(bool.not.symbol)
          val b2 = s.check.get
          if (b1 & b2) {
            val b1 = b.clone
            b1.ccr.ccr = b.ccr.setC
            b1.path.set(bool.symbol)
            ans += b1
            val b2 = b.clone
            b2.ccr.ccr = b.ccr.clearC
            b2.path.set(bool.not.symbol)
            ans += b2
          } else if (b1) {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.setC
            ans += bc
          }
          else {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.clearC
            ans += bc
          }
      }
    }
    ans
  }

  def checkV(data1: MySymbol, data2: MySymbol, res: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    val and =
      size match {
        case 8 => 0x80
        case 16 => 0x8000
        case 32 => 0x80000000
      }
    buf.foreach { b =>
      (data1, data2, res) match {
        case (d1: IntSymbol, d2: IntSymbol, r: IntSymbol) =>
          val bc = b.clone
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and) && (r & and).eq(0)
          val bool2 = (d1 & and).eq(0) && (d2 & and).eq(0) && (r & and).eq(and)
          bc.ccr.ccr = (if (bool1 || bool2) bc.ccr.setV else bc.ccr.clearV)
          ans += bc
        case _ =>
          val s = ctx.mkSolver
          s.push
          val bool1 = (data1 & and).eq(new IntSymbol(and)) && (data2 & and).eq(new IntSymbol(and)) && (res & and).eq(new IntSymbol(0))
          val bool2 = (data1 & and).eq(new IntSymbol(0)) && (data2 & and).eq(new IntSymbol(0)) && (res & and).eq(new IntSymbol(and))
          val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
          s.assertCnstr(bool.symbol)
          val b1 = s.check.get
          s.pop(1)
          s.assertCnstr(bool.not.symbol)
          val b2 = s.check.get
          if (b1 & b2) {
            val b1 = b.clone
            b1.ccr.ccr = b.ccr.setV
            b1.path.set(bool.symbol)
            ans += b1
            val b2 = b.clone
            b2.ccr.ccr = b.ccr.clearV
            b2.path.set(bool.not.symbol)
            ans += b2
          } else if (b1) {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.setV
            ans += bc
          }
          else {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.clearV
            ans += bc
          }
      }
    }
    ans
  }

  def checkZ(data: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    val and =
      size match {
        case 8 => 0xFF
        case 16 => 0xFFFF
        case 32 => 0xFFFFFFFF
      }
    buf.foreach { b =>
      data match {
        case d: IntSymbol =>
          val bc = b.clone
          bc.ccr.ccr = (if ((d & and).symbol == 0) bc.ccr.setZ else bc.ccr.clearZ)
          ans += bc
        case d: CtxSymbol =>
          val s = ctx.mkSolver
          s.push
          val eq = d.&(and).eq(0)
          s.assertCnstr(eq.symbol)
          val b1 = s.check.get
          s.pop(1)
          s.assertCnstr(eq.not.symbol)
          val b2 = s.check.get
          if (b1 & b2) {
            val b1 = b.clone
            b1.ccr.ccr = b.ccr.setZ
            b1.path.set(eq.symbol)
            ans += b1
            val b2 = b.clone
            b2.ccr.ccr = b.ccr.clearZ
            b2.path.set(eq.not.symbol)
            ans += b2
          } else if (b1) {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.setZ
            ans += bc
          }
          else {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.clearZ
            ans += bc
          }
      }
    }
    ans
  }

  def checkN(data: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    val and =
      size match {
        case 8 => 0x80
        case 16 => 0x8000
        case 32 => 0x80000000
      }
    buf.foreach { b =>
      data match {
        case d: IntSymbol =>
          val bc = b.clone
          bc.ccr.ccr = (if ((d & and).symbol == and) bc.ccr.setN else bc.ccr.clearN)
          ans += bc
        case d: CtxSymbol =>
          val s = ctx.mkSolver
          s.push
          val ge = (d & and).eq(and)
          s.assertCnstr(ge.symbol)
          val b1 = s.check.get
          s.pop(1)
          s.assertCnstr(ge.not.symbol)
          val b2 = s.check.get
          if (b1 & b2) {
            val b1 = b.clone
            b1.ccr.ccr = b.ccr.setN
            b1.path.set(ge.symbol)
            ans += b1
            val b2 = b.clone
            b2.ccr.ccr = b.ccr.clearN
            b2.path.set(ge.not.symbol)
            ans += b2
          } else if (b1) {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.setN
            ans += bc
          }
          else {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.clearN
            ans += bc
          }
      }
    }
    ans
  }

  def checkH(data1: MySymbol, data2: MySymbol, res: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    val and =
      size match {
        case 8 => 0x08
        case 16 => 0x0800
        case 32 => 0x00800000
      }
    buf.foreach { b =>
      (data1, data2, res) match {
        case (d1: IntSymbol, d2: IntSymbol, r: IntSymbol) =>
          val bc = b.clone
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and)
          val bool2 = ((d1 & and).eq(and) || (d2 & and).eq(and)) && (r & and).eq(and)
          bc.ccr.ccr = (if (bool1 || bool2) bc.ccr.setH else bc.ccr.clearH)
          ans += bc
        case _ =>
          val s = ctx.mkSolver
          s.push
          val bool1 = (data1 & and).eq(new IntSymbol(and)) && (data2 & and).eq(new IntSymbol(and))
          val bool2 = ((data1 & and).eq(new IntSymbol(and)) || (data2 & and).eq(new IntSymbol(and))) && ((res & and).eq(new IntSymbol(and)))
          val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
          s.assertCnstr(bool.symbol)
          val b1 = s.check.get
          s.pop(1)
          s.assertCnstr(bool.not.symbol)
          val b2 = s.check.get
          if (b1 & b2) {
            val b1 = b.clone
            b1.ccr.ccr = b.ccr.setH
            b1.path.set(bool.symbol)
            ans += b1
            val b2 = b.clone
            b2.ccr.ccr = b.ccr.clearH
            b2.path.set(bool.not.symbol)
            ans += b2
          } else if (b1) {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.setH
            ans += bc
          }
          else {
            val bc = b.clone
            bc.ccr.ccr = bc.ccr.clearH
            ans += bc
          }
      }
    }
    ans
  }

}
