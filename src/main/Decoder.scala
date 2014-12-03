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

  def analyze(data: DataSet): ArrayBuffer[DataSet] = {
    decode(data.clone, data.pc.pc)
  }

  private def decode(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    data.mem.getByte(pc) match {
      case op0: IntSymbol => decodeInt(data, pc)
      //      case op0: CtxSymbol => decodeSymbol(data, pc)
    }
  }

  private def decodeInt(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    val op0 = data.mem.getByte(pc).asInstanceOf[IntSymbol].symbol
    op0 & 0xF0 match {
      case 0x00 => buf ++= analyze0(data, pc)
      case 0x10 => buf ++= analyze1(data, pc)
      case 0x20 =>
        //MOV.B Abs,Reg [2reg][abs]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val mov = data.mem.getByte(op1)
        data.reg.setByte(mov, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        buf += data
        buf = checkZ(mov, buf, 8)
        buf = checkN(mov, buf, 8)
      case 0x30 =>
        //MOV.B Reg,Abs [3reg][abs]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val mov = data.reg.getByte(op0)
        data.reg.setByte(mov, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        buf += data
        buf = checkZ(mov, buf, 8)
        buf = checkN(mov, buf, 8)
      case 0x40 => buf ++= analyze4(data, pc)
      case 0x50 => buf ++= analyze5(data, pc)
      case 0x60 => buf ++= analyze6(data, pc)
      case 0x70 => buf ++= analyze7(data, pc)
      case 0x80 =>
        //ADD.B Imm,Reg [8Reg][Imm]
        val imm = data.mem.getByte(pc + 1)
        val reg = data.reg.getByte(op0)
        val add = reg + imm
        data.reg.setByte(add, op0)
        data.pc.setPc(pc + 2)
        buf += data
        buf = checkC(reg, imm, add, buf, 8)
        buf = checkV(reg, imm, add, buf, 8)
        buf = checkZ(add, buf, 8)
        buf = checkN(add, buf, 8)
        buf = checkH(reg, imm, add, buf, 8)
      case 0xA0 =>
        //CMP.B Imm,Reg [Areg][imm]
        val imm = data.mem.getByte(pc + 1)
        val reg = data.reg.getByte(op0)
        val cmp = reg - imm
        data.pc.setPc(pc + 2)
        buf += data
        buf = checkC(reg, imm.neg, cmp, buf, 8)
        buf = checkV(reg, imm.neg, cmp, buf, 8)
        buf = checkZ(cmp, buf, 8)
        buf = checkN(cmp, buf, 8)
        buf = checkH(reg, imm.neg, cmp, buf, 8)
      case 0xE0 =>
        //AND.B Imm,Reg [Ereg][Imm]
        val imm = data.mem.getByte(pc + 1)
        val and = data.reg.getByte(op0) & imm
        data.reg.setByte(and, op0)
        data.ccr.clearV
        data.pc.setPc(pc + 2)
        buf += data
        buf = checkZ(imm, buf, 8)
        buf = checkN(imm, buf, 8)
      case 0xF0 =>
        //MOV.B Imm,Reg [Freg][Imm]
        val imm = data.mem.getByte(pc + 1)
        data.reg.setByte(imm, op0)
        data.ccr.clearV
        data.pc.setPc(pc + 2)
        buf += data
        buf = checkZ(imm, buf, 8)
        buf = checkN(imm, buf, 8)
    }
    buf
  }

  private def analyze0(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = data.mem.getByte(pc).asInstanceOf[IntSymbol].symbol
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x04 =>
        //ORC.B Imm,Ccr [04][imm]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val imm = data.mem.getByte(op1)
        data.ccr.ccr = data.ccr.ccr | imm
        data.pc.setPc(pc + 2)
        buf += data
      case 0x06 =>
        //ANDC.B Imm,Ccr [06][imm]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val imm = data.mem.getByte(op1)
        data.ccr.ccr = data.ccr.ccr & imm
        data.pc.setPc(pc + 2)
        buf += data
      case 0x0B =>
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        op1 & 0xF0 match {
          case 0x50 =>
            //INC.W #1,Reg [0B][5reg]
            val reg = data.reg.getWord(op1)
            val inc = reg + 1
            data.reg.setWord(inc, op1)
            data.pc.setPc(pc + 2)
            buf += data
            buf = checkV(reg, new IntSymbol(1), inc, buf, 8)
            buf = checkZ(inc, buf, 8)
            buf = checkN(inc, buf, 8)
        }
      case 0x0D =>
        //MOV.W RegA,RegB [0D][regAregB]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val regA = data.reg.getByte(op1 >> 4)
        data.reg.setWord(regA, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        buf += data
        buf = checkZ(regA, buf, 16)
        buf = checkN(regA, buf, 16)
    }
    buf
  }

  private def analyze1(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = data.mem.getByte(pc).asInstanceOf[IntSymbol].symbol
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x17 =>
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        op1 & 0xF0 match {
          case 0x00 =>
            //NOT.B Reg [17][0reg]
            val reg = data.reg.getByte(op1)
            val not = reg.~
            data.reg.setByte(not, op1)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            buf += data
            buf = checkZ(not, buf, 8)
            buf = checkN(not, buf, 8)
          case 0x50 =>
            //EXTU.W Reg [17][5reg]
            val reg = data.reg.getWord(op1)
            val extu = reg & 0xFF
            data.reg.setWord(extu, op1)
            data.pc.setPc(pc + 2)
            data.ccr.clearN
            data.ccr.clearV
            buf += data
            buf = checkZ(extu, buf, 8)
        }
      case 0x18 =>
        //SUB.B RegA,RegB [18][regAregB]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val sub = regB - regA
        data.reg.setByte(sub, op1)
        data.pc.setPc(pc + 2)
        buf += data
        buf = checkC(regB, regA.neg, sub, buf, 8)
        buf = checkV(regB, regA.neg, sub, buf, 8)
        buf = checkZ(sub, buf, 8)
        buf = checkN(sub, buf, 8)
        buf = checkH(regB, regA.neg, sub, buf, 8)
      case 0x19 =>
        //SUB.W RegA,RegB [19][regAregB]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val sub = regB - regA
        data.reg.setWord(sub, op1)
        data.pc.setPc(pc + 2)
        buf += data
        buf = checkC(regB, regA.neg, sub, buf, 16)
        buf = checkV(regB, regA.neg, sub, buf, 16)
        buf = checkZ(sub, buf, 16)
        buf = checkN(sub, buf, 16)
        buf = checkH(regB, regA.neg, sub, buf, 16)
      case 0x1D =>
        //CMP.W RegA,RegB [1D][regAregB]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val sub = regB - regA
        data.pc.setPc(pc + 2)
        buf += data
        buf = checkC(regB, regA.neg, sub, buf, 16)
        buf = checkV(regB, regA.neg, sub, buf, 16)
        buf = checkZ(sub, buf, 16)
        buf = checkN(sub, buf, 16)
        buf = checkH(regB, regA.neg, sub, buf, 16)
    }
    buf
  }

  private def analyze4(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = data.mem.getByte(pc).asInstanceOf[IntSymbol].symbol
    var buf = new ArrayBuffer[DataSet]
    val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
    val disp =
      if ((op1 & 0x80) == 0x80) op1 | 0xFF00
      else op1
    op0 match {
      //Bcc [4X][disp]
      case 0x40 =>
        //BRA true
        data.pc.pc = pc + disp
        buf += data
      case 0x42 =>
        //BHI C|V = 0
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            val z = (ccr & 0x04).eq(0x04)
            if (!(c | z)) data.pc.pc = pc + disp
            else data.pc.pc += 2
            buf += data
          case ccr: CtxSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            val z = (ccr & 0x04).eq(0x04)
            val or = ctx.mkAnd(c.symbol, z.symbol)
            val dc1 = data.clone
            dc1.pc.pc = pc + 2
            dc1.path.set(or)
            buf += dc1
            val dc2 = data.clone
            dc2.pc.pc = pc + disp
            dc2.path.set(ctx.mkNot(or))
            buf += dc2
        }
        data.pc.pc = pc + disp
      case 0x45 =>
        //BLO C = 1
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            if (c) data.pc.pc = pc + disp
            else data.pc.pc += 2
          case ccr: CtxSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            val dc1 = data.clone
            dc1.pc.pc = pc + disp
            dc1.path.set(c.symbol)
            buf += dc1
            val dc2 = data.clone
            dc2.pc.pc = pc + 2
            dc1.path.set(c.not.symbol)
            buf += dc2
        }
      case 0x4D =>
        //BLT N 排他的論理和 V = 1
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val n = (ccr & 0x08).eq(0x08)
            val v = (ccr & 0x02).eq(0x02)
            if (n ^ v) data.pc.pc = pc + disp
            else data.pc.pc += 2
          case ccr: CtxSymbol =>
            val n = (ccr & 0x08).eq(0x08)
            val v = (ccr & 0x02).eq(0x02)
            val xor = ctx.mkBVXor(n.symbol, v.symbol)
            val dc1 = data.clone
            dc1.pc.pc = pc + disp
            dc1.path.set(xor)
            buf += dc1
            val dc2 = data.clone
            dc2.pc.pc = pc + 2
            dc2.path.set(ctx.mkNot(xor))
            buf += dc2
        }
    }
    buf
  }

  private def analyze5(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = data.mem.getByte(pc).asInstanceOf[IntSymbol].symbol
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x54 =>
        //RTS [54][70]
        val sp = data.reg.getLong(7)
        val d = data.mem.getWord(sp.asInstanceOf[IntSymbol].symbol)
        data.pc.setPc(d.asInstanceOf[IntSymbol].symbol)
        data.reg.setLong(sp + 2, 7)
        buf += data
      case 0x56 =>
        //RTE [56][70]
        val sp = data.reg.getLong(7)
        val d = data.mem.getLong(sp.asInstanceOf[IntSymbol].symbol)
        val setpc = d & 0xFFFF
        val ccr = (d >> 24) & 0xFF
        data.ccr.ccr = ccr
        data.pc.setPc(setpc.asInstanceOf[IntSymbol].symbol)
        buf += data
      case 0x59 =>
        //JMP IndReg [59][reg0]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val reg = data.reg.getLong(op1 >> 4)
        data.pc.setPc(reg.asInstanceOf[IntSymbol].symbol)
        buf += data
      case 0x5E =>
        //JSR Abs:24 [5E][Abs][Abs][Abs]
        val abs = data.mem.getLong(pc)
        val sp = data.reg.getLong(7)
        val stock = data.pc.pc + 4
        data.mem.setWord(new IntSymbol(stock), (sp - 2).asInstanceOf[IntSymbol].symbol)
        data.reg.setLong(sp - 2, 7)
        data.pc.setPc(abs.asInstanceOf[IntSymbol].symbol)
        buf += data
    }
    buf
  }

  private def analyze6(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = data.mem.getByte(pc).asInstanceOf[IntSymbol].symbol
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x68 =>
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        op1 & 0x80 match {
          case 0x00 =>
            //MOV.B IndReg,Reg [68][0indreg reg]
            val abs = data.reg.getLong(op1 >> 4)
            val mov = data.mem.getByte(abs)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            mov.foreach { m =>
              val d = data.clone
              d.reg.setByte(m, op1)
              var buf1 = ArrayBuffer[DataSet](d)
              buf1 = checkZ(m, buf1, 8)
              buf1 = checkN(m, buf1, 8)
              buf ++= buf1
            }
          case 0x80 =>
            //MOV,B Reg,IndReg [68][1indreg reg]
            val mov = data.reg.getByte(op1)
            val abs = data.reg.getLong(op1 >> 4)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            abs match {
              case a: IntSymbol =>
                data.mem.setByte(mov, a.symbol)
                buf += data
              case a: CtxSymbol => Main.extract(0 to 0xFFFF, a.symbol).foreach { add =>
                val d = data.clone
                d.mem.setByte(mov, add)
                buf += d
              }
            }
            buf = checkZ(mov, buf, 8)
            buf = checkN(mov, buf, 8)
        }
      case 0x6A =>
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        op1 & 0xF0 match {
          case 0x80 =>
            //MOV.B Reg,Abs [6A][8reg][abs][abs]
            val mov = data.reg.getByte(op1)
            val absh = (data.mem.getByte(pc + 2) & 0xFF) << 8
            val abs = ((data.mem.getByte(pc + 2) & 0xFF) | absh)
            data.mem.setByte(mov, abs.asInstanceOf[IntSymbol].symbol)
            data.pc.setPc(pc + 4)
            data.ccr.clearV
            buf += data
            buf = checkZ(mov, buf, 8)
            buf = checkN(mov, buf, 8)
        }
      case 0x6B =>
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV.W Abs,Reg [6B][0Reg][abs][abs]
            val abs = data.mem.getWord(pc + 2)
            val mov = data.mem.getWord(abs.asInstanceOf[IntSymbol].symbol)
            data.reg.setWord(mov, op1)
            data.pc.setPc(pc + 4)
            data.ccr.clearV
            buf += data
            buf = checkZ(mov, buf, 16)
            buf = checkN(mov, buf, 16)
          case 0x80 =>
            //MOV.W Reg,Abs [6B][8reg][abs][abs]
            val abs = data.mem.getWord(pc + 2)
            val mov = data.reg.getWord(op1)
            data.mem.setWord(mov, abs.asInstanceOf[IntSymbol].symbol)
            data.pc.setPc(pc + 4)
            data.ccr.clearV
            buf += data
            buf = checkZ(mov, buf, 16)
            buf = checkN(mov, buf, 16)
        }
      case 0x6D =>
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        op1 & 0xF0 match {
          case 0x70 =>
            //POP.W Reg [6D][7Reg]
            val sp = data.reg.getLong(7)
            val pop = data.mem.getWord(sp.asInstanceOf[IntSymbol].symbol)
            data.reg.setWord(pop, op1)
            data.reg.setLong(sp + 2, 7)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            buf += data
            buf = checkZ(pop, buf, 16)
            buf = checkN(pop, buf, 16)
          case 0xF0 =>
            //PUSH.W Reg [6D][FReg]
            val push = data.reg.getWord(op1)
            val sp = data.reg.getLong(7)
            data.mem.setWord(push, (sp - 2).asInstanceOf[IntSymbol].symbol)
            data.reg.setLong(sp - 2, 7)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            buf += data
            buf = checkZ(push, buf, 16)
            buf = checkN(push, buf, 16)
        }
      case 0x6E =>
        //MOV.B Disp,Reg [6E][dregreg][disp][disp]
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        val disp = data.mem.getWord(pc + 2)
        val dreg = data.reg.getLong(op1 >> 4)
        val add = dreg + disp
        data.pc.setPc(pc + 4)
        data.ccr.clearV
        val mov = data.mem.getByte(add)
        mov.foreach { m =>
          val d = data.clone
          d.reg.setByte(m, op1)
          var buf1 = ArrayBuffer[DataSet](d)
          buf1 = checkZ(m, buf1, 8)
          buf1 = checkN(m, buf1, 8)
          buf ++= buf1
        }
    }
    buf
  }

  private def analyze7(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = data.mem.getByte(pc).asInstanceOf[IntSymbol].symbol
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x79 =>
        val op1 = data.mem.getByte(pc + 2).asInstanceOf[IntSymbol].symbol
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV.W Imm,Reg [79][0reg][imm][imm]
            val imm = data.mem.getWord(pc + 2)
            data.reg.setWord(imm, op1)
            data.pc.setPc(pc + 4)
            data.ccr.clearV
            buf += data
            buf = checkZ(imm, buf, 16)
            buf = checkN(imm, buf, 16)
          case 0x10 =>
            //ADD.W Imm,Reg [79][1reg][imm][imm]
            val imm = data.mem.getWord(pc + 2)
            val reg = data.reg.getWord(op1)
            val add = reg + imm
            data.reg.setWord(add, op1)
            data.pc.setPc(pc + 4)
            buf += data
            buf = checkC(reg, imm, add, buf, 16)
            buf = checkV(reg, imm, add, buf, 16)
            buf = checkZ(reg, buf, 16)
            buf = checkN(reg, buf, 16)
            buf = checkH(reg, imm, add, buf, 16)
          case 0x20 =>
            //CMP.W Imm,Reg [79][2reg][imm][imm]
            val imm = data.mem.getWord(pc + 2)
            val reg = data.reg.getWord(op1)
            val cmp = reg - imm
            data.pc.setPc(pc + 4)
            buf += data
            buf = checkC(reg, imm.neg, cmp, buf, 16)
            buf = checkV(reg, imm.neg, cmp, buf, 16)
            buf = checkZ(cmp, buf, 16)
            buf = checkN(cmp, buf, 16)
            buf = checkH(reg, imm.neg, cmp, buf, 16)
        }
      case 0x7A =>
        val op1 = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV,L Imm,Reg [7A][0reg][imm][imm][imm][imm]
            val imm = data.mem.getLong(pc + 2)
            data.reg.setLong(imm, op1)
            data.pc.setPc(pc + 6)
            data.ccr.clearV
            buf += data
            buf = checkZ(imm, buf, 32)
            buf = checkN(imm, buf, 32)
        }
      case 0x7F =>
        val op2 = data.mem.getByte(pc + 2).asInstanceOf[IntSymbol].symbol
        op2 match {
          case 0x70 =>
            //BSET.B Imm,Abs [7F][Abs][70][Imm0]
            val abs = data.mem.getByte(pc + 1).asInstanceOf[IntSymbol].symbol
            val imm = data.mem.getByte(pc + 3) >> 4
            val bset = data.mem.getByte(abs).bitset(imm)
            data.mem.setByte(bset, abs)
            data.pc.setPc(pc + 4)
            buf += data
        }
    }
    buf
  }

  private def checkC(data1: MySymbol, data2: MySymbol, res: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
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
          if (bool1 || bool2) bc.ccr.setC else bc.ccr.clearC
          ans += bc
        case _ =>

          val bool1 = (data1 & and).eq(new IntSymbol(and)) && (data2 & and).eq(new IntSymbol(and))
          val bool2 = ((data1 & and).eq(new IntSymbol(and)) || (data2 & and).eq(new IntSymbol(and))) && ((res & and).eq(new IntSymbol(and)))
          val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
          val bc1 = b.clone
          bc1.ccr.setC
          bc1.path.set(bool.symbol)
          ans += bc1
          val bc2 = b.clone
          bc2.ccr.clearC
          bc2.path.set(bool.not.symbol)
          ans += bc2
      }
    }
    ans
  }

  private def checkV(data1: MySymbol, data2: MySymbol, res: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
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
          if (bool1 || bool2) bc.ccr.setV else bc.ccr.clearV
          ans += bc
        case _ =>
          val bool1 = (data1 & and).eq(new IntSymbol(and)) && (data2 & and).eq(new IntSymbol(and)) && (res & and).eq(new IntSymbol(0))
          val bool2 = (data1 & and).eq(new IntSymbol(0)) && (data2 & and).eq(new IntSymbol(0)) && (res & and).eq(new IntSymbol(and))
          val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
          val bc1 = b.clone
          bc1.ccr.setV
          bc1.path.set(bool.symbol)
          ans += bc1
          val bc2 = b.clone
          bc2.ccr.clearV
          bc2.path.set(bool.not.symbol)
          ans += bc2
      }
    }
    ans
  }

  private def checkZ(data: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
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
          if ((d & and).symbol == 0) bc.ccr.setZ else bc.ccr.clearZ
          ans += bc
        case d: CtxSymbol =>
          val eq = d.&(and).eq(0)
          val bc1 = b.clone
          bc1.ccr.setZ //data = 0
          bc1.path.set(eq.symbol)
          ans += bc1
          val bc2 = b.clone
          bc2.ccr.clearZ
          bc2.path.set(eq.not.symbol)
          ans += bc2
      }
    }
    ans
  }

  private def checkN(data: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
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
          if ((d & and).symbol == and) bc.ccr.setN else bc.ccr.clearN
          ans += bc
        case d: CtxSymbol =>
          val ge = (d & and).eq(and)
          val bc1 = b.clone
          bc1.ccr.setN //data < 0
          bc1.path.set(ge.not.symbol)
          ans += bc1
          val bc2 = b.clone
          bc2.ccr.clearN
          bc2.path.set(ge.symbol)
          ans += bc2
      }
    }
    ans
  }

  private def checkH(data1: MySymbol, data2: MySymbol, res: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
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
          if (bool1 || bool2) bc.ccr.setH else bc.ccr.clearH
          ans += bc
        case _ =>
          val bool1 = (data1 & and).eq(new IntSymbol(and)) && (data2 & and).eq(new IntSymbol(and))
          val bool2 = ((data1 & and).eq(new IntSymbol(and)) || (data2 & and).eq(new IntSymbol(and))) && ((res & and).eq(new IntSymbol(and)))
          val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
          val bc1 = b.clone
          bc1.ccr.setH
          bc1.path.set(bool.symbol)
          ans += bc1
          val bc2 = b.clone
          bc2.ccr.clearH
          bc2.path.set(bool.not.symbol)
          ans += bc2
      }
    }
    ans
  }

}
