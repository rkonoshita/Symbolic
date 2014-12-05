package main

import data.DataSet
import data.register.ROM
import symbol.CtxSymbol
import z3.scala.Z3Context

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/17.
 */
class Decoder(c: Z3Context, r: ROM) {

  private val ctx = c
  private val rom = r

  def analyze(data: DataSet): ArrayBuffer[DataSet] = {
    val buf = new ArrayBuffer[DataSet]
    decode(data, data.pc.pc)
  }

  private def decode(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    val op0 = rom.readByte(pc)
    op0 & 0xF0 match {
      case 0x00 => buf ++= analyze0(data, pc)

      case 0x01 => buf ++= analyze1(data, pc)

      case 0x20 =>
        //MOV.B Abs,Reg [2reg][abs]
        val abs = 0xFFFF | rom.readByte(pc + 1)
        val mov = data.mem.getByte(abs)
        data.ccr.clearV
        data.pc.pc += 2
        data.reg.setByte(mov, op0)
        buf = checkZ(mov, buf)
        buf = checkN(mov, buf)

      case 0x30 =>
        //MOV.B Reg,Abs [3reg][abs]
        val abs = rom.readByte(pc + 1)
        val mov = data.reg.getByte(op0)
        data.ccr.clearV
        data.pc.pc += 2
        data.mem.setByte(mov, abs)
        buf = checkZ(mov, ArrayBuffer(data))
        buf = checkN(mov, buf)

      case 0x40 => buf ++= analyze4(data, pc)

      case 0x50 => buf ++= analyze5(data, pc)

      case 0x60 => buf ++= analyze6(data, pc)

      case 0x70 => buf ++= analyze7(data, pc)

      case 0x80 =>
        //ADD.B Imm,Reg [8Reg][Imm]
        val imm = new CtxSymbol(ctx, rom.readByte(pc + 1), 8)
        val reg = data.reg.getByte(op0)
        val add = reg + imm
        data.reg.setByte(add, op0)
        data.pc.pc += 2
        buf = checkC(reg, imm, add, ArrayBuffer(data))
        buf = checkV(reg, imm, add, buf)
        buf = checkZ(add, buf)
        buf = checkN(add, buf)
        buf = checkH(reg, imm, add, buf)

      case 0xA0 =>
        //CMP.B Imm,Reg [Areg][imm]
        val imm = new CtxSymbol(ctx, rom.readByte(pc + 1), 8)
        val reg = data.reg.getByte(op0)
        val cmp = reg - imm
        data.pc.pc += 2
        buf = checkC(reg, imm.neg, cmp, ArrayBuffer(data))
        buf = checkV(reg, imm.neg, cmp, buf)
        buf = checkZ(cmp, buf)
        buf = checkN(cmp, buf)
        buf = checkH(reg, imm.neg, cmp, buf)

      case 0xE0 =>
        //AND.B Imm,Reg [Ereg][Imm]
        val and = data.reg.getByte(op0) & rom.readByte(pc + 1)
        data.reg.setByte(and, op0)
        data.ccr.clearV
        data.pc.pc += 2
        buf = checkZ(and, ArrayBuffer(data))
        buf = checkN(and, buf)

      case 0xF0 =>
        //MOV.B Imm,Reg [Freg][Imm]
        val mov = new CtxSymbol(ctx, rom.readByte(pc + 1), 8)
        data.reg.setByte(mov, op0)
        data.ccr.clearV
        data.pc.pc += 2
        buf = checkZ(mov, ArrayBuffer(data))
        buf ++= checkN(mov, buf)
    }
    buf
  }

  private def analyze0(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    rom.readByte(0) match {
      case 0x04 =>
        //ORC.B Imm,Ccr [04][imm]
        val imm = rom.readByte(pc + 1)
        data.ccr.ccr = data.ccr.ccr | imm
        data.pc.pc += 2
        buf += data

      case 0x06 =>
        //ANDC.B Imm,Ccr [06][imm]
        val imm = rom.readByte(pc + 1)
        data.ccr.ccr = data.ccr.ccr & imm
        data.pc.pc += 2
        buf += data

      case 0x0B =>
        val op1 = rom.readByte(pc + 1)
        op1 & 0xF0 match {
          case 0x50 =>
            //INC.W #1,Reg [0B][5reg]
            val reg = data.reg.getWord(op1)
            val inc = reg + 1
            data.reg.setWord(inc, op1)
            data.pc.pc += 2
            buf = checkV(reg, new CtxSymbol(ctx, 1, 16), inc, ArrayBuffer(data))
            buf = checkZ(inc, buf)
            buf = checkN(inc, buf)
        }

      case 0x0D =>
        //MOV.W RegA,RegB [0D][regAregB]
        val op1 = rom.readByte(pc + 1)
        val regA = data.reg.getWord(op1)
        data.pc.pc += 2
        data.ccr.clearV
        data.reg.setWord(regA, op1)
        buf = checkZ(regA, ArrayBuffer(data))
        buf = checkN(regA, buf)
    }
    buf
  }

  private def analyze1(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    rom.readByte(0) match {
      case 0x17 =>
        val op1 = rom.readByte(pc + 1)
        op1 & 0xF0 match {
          case 0x00 =>
            //NOT.B Reg [17][0reg]
            val reg = data.reg.getByte(op1)
            val not = reg.~
            data.reg.setByte(not, op1)
            data.pc.pc += 2
            data.ccr.clearV
            buf = checkZ(not, ArrayBuffer(data))
            buf = checkN(not, buf)

          case 0x50 =>
            //EXTU.W Reg [17][5reg]
            val reg = data.reg.getByte(op1)
            val extu = new CtxSymbol(ctx, 0, 8) :: reg
            data.reg.setWord(extu, op1)
            data.pc.pc += 2
            data.ccr.clearN
            data.ccr.clearV
            buf = checkZ(extu, ArrayBuffer(data))
        }

      case 0x18 =>
        //SUB.B RegA,RegB [18][regAregB]
        val op1 = rom.readByte(pc + 1)
        val regA = data.reg.getByte(op1)
        val regB = data.reg.getByte(op1)
        val sub = regB - regA
        data.pc.pc += 2
        data.reg.setByte(sub, op1)
        buf = checkC(regB, regA.neg, sub, ArrayBuffer(data))
        buf = checkV(regB, regA.neg, sub, buf)
        buf = checkZ(sub, buf)
        buf = checkN(sub, buf)
        buf = checkH(regB, regA.neg, sub, buf)

      case 0x19 =>
        //SUB.W RegA,RegB [19][regAregB]
        val op1 = rom.readByte(pc + 1)
        val regA = data.reg.getWord(op1)
        val regB = data.reg.getWord(op1)
        val sub = regB - regA
        data.pc.pc += 2
        data.reg.setWord(sub, op1)
        buf = checkC(regB, regA.neg, sub, ArrayBuffer(data))
        buf = checkV(regB, regA.neg, sub, buf)
        buf = checkZ(sub, buf)
        buf = checkN(sub, buf)
        buf = checkH(regB, regA.neg, sub, buf)

      case 0x1D =>
        //CMP.W RegA,RegB [1D][regAregB]
        val op1 = rom.readByte(pc + 1)
        val regA = data.reg.getWord(op1)
        val regB = data.reg.getWord(op1)
        val sub = regB - regA
        data.pc.pc += 2
        buf = checkC(regB, regA.neg, sub, ArrayBuffer(data))
        buf = checkV(regB, regA.neg, sub, buf)
        buf = checkZ(sub, buf)
        buf = checkN(sub, buf)
        buf = checkH(regB, regA.neg, sub, buf)
    }
    buf
  }

  private def analyze4(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    val op1 = rom.readByte(pc + 1)
    val disp =
      if ((op1 & 0x80) == 0x80) op1 | 0xFFFF00
      else op1
    rom.readByte(pc) match {
      //Bcc [4X][disp]
      case 0x40 =>
        //BRA true
        data.pc.pc = disp + pc
        buf += data

      case 0x42 =>
        //BHI C|V = 0
        val c = (data.ccr.ccr & 0x01).eq(0x01)
        val z = (data.ccr.ccr & 0x04).eq(0x04)
        val or = ctx.mkOr(c.symbol, z.symbol)
        val dc1 = data.clone
        dc1.pc.pc += 2
        dc1.path.set(or)
        buf += dc1
        val dc2 = data.clone
        dc2.pc.pc = disp + pc
        dc2.path.set(ctx.mkNot(or))
        buf += dc2

      case 0x45 =>
        //BLO C = 1
        val c = (data.ccr.ccr & 0x01).eq(0x01)
        val dc1 = data.clone
        dc1.pc.pc = disp + pc
        dc1.path.set(c.symbol)
        buf += dc1
        val dc2 = data.clone
        dc2.pc.pc += 2
        dc2.path.set(c.not.symbol)
        buf += dc2

      case 0x4D =>
        //BLT N 排他的論理和 V = 1
        val n = (data.ccr.ccr & 0x08).eq(0x08)
        val v = (data.ccr.ccr & 0x02).eq(0x02)
        val xor = ctx.mkBVXor(n.symbol, v.symbol)
        val dc1 = data.clone
        dc1.pc.pc = pc + 2
        dc1.path.set(xor)
        buf += dc1
        val dc2 = data.clone
        dc2.pc.pc = disp + pc
        dc2.path.set(ctx.mkNot(xor))
        buf += dc2
    }
    buf
  }

  private def analyze5(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    rom.readByte(pc) match {
      case 0x54 =>
        //RTS [54][70]
        val sp = data.reg.getLong(7)
        val pop = data.mem.getWord(sp)
        data.reg.setLong(sp + 2, 7)
        pop.foreach { p =>
          Main.extract(0 to 0xFFFF, p).foreach { set =>
            val clone = data.clone
            clone.pc.pc = set
            buf += clone
          }
        }

      case 0x56 =>
        //RTE [56][70]
        val sp = data.reg.getLong(7)
        val pop = data.mem.getLong(sp)
        data.reg.setLong(sp + 4, 7)
        pop.foreach { p =>
          val clone = data.clone
          val setpc = p.extract(15, 0)
          val ccr = p.extract(31, 24)
          clone.ccr.ccr = ccr
          Main.extract(0 to 0xFFFF, p).foreach { set =>
            val wclone = clone.clone
            wclone.pc.pc = set
            buf += wclone
          }
        }

      case 0x59 =>
        //JMP IndReg [59][reg0]
        val op1 = rom.readByte(pc + 1)
        val reg = data.reg.getLong(op1 >> 4)
        Main.extract(0 to 0xFFFF, reg).foreach { r =>
          val clone = data.clone
          clone.pc.pc = r
          buf += clone
        }

      case 0x5E =>
        //JSR Abs:24 [5E][Abs][Abs][Abs]
        val abs = rom.readLong(pc) & 0x00FFFFFF
        val sp = data.reg.getLong(7)
        val stock = data.pc.pc + 4
        data.reg.setLong(sp - 2, 7)
        data.pc.pc = abs
        Main.extract(0 to 0xFFFF, sp).foreach { s =>
          val clone = data.clone
          clone.mem.setWord(new CtxSymbol(ctx, stock, 16), s - 2)
          buf += clone
        }
    }
    buf
  }

  private def analyze6(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    rom.readByte(pc) match {
      case 0x68 =>
        val op1 = rom.readByte(pc + 1)
        op1 & 0x80 match {
          case 0x00 =>
            //MOV.B IndReg,Reg [68][0indreg reg]
            val abs = data.reg.getLong(op1).extract(23, 0)
            val mov = data.mem.getByte(abs)
            data.pc.pc += 2
            data.ccr.clearV
            mov.foreach { m =>
              val clone = data.clone
              clone.reg.setByte(m, op1)
              var buff = ArrayBuffer(clone)
              buff = checkZ(m, buff)
              buf ++= checkN(m, buff)
            }

          case 0x80 =>
            //MOV,B Reg,IndReg [68][1indreg reg]
            val mov = data.reg.getByte(op1)
            val abs = data.reg.getLong(op1 >> 4).extract(23, 0)
            data.pc.pc += 2
            data.ccr.clearV
            Main.extract(0 to 0xFFFF, abs).foreach { a =>
              val clone = data.clone
              clone.mem.setByte(mov, a)
              var buff = ArrayBuffer(clone)
              buff = checkZ(mov, buff)
              buf ++= checkN(mov, buff)
            }
        }

      case 0x6A =>
        val op1 = rom.readByte(pc + 1)
        op1 & 0xF0 match {
          case 0x80 =>
            //MOV.B Reg,Abs [6A][8reg][abs][abs]
            val mov = data.reg.getByte(op1)
            val abs = rom.readWord(pc + 2)
            data.mem.setByte(mov, abs)
            data.pc.pc += 4
            data.ccr.clearV
            buf = checkZ(mov, ArrayBuffer(data))
            buf = checkN(mov, buf)
        }

      case 0x6B =>
        val op1 = rom.readByte(pc + 1)
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV.W Abs,Reg [6B][0Reg][abs][abs]
            val abs = rom.readWord(pc + 2)
            val mov = data.mem.getWord(abs)
            data.reg.setWord(mov, op1)
            data.pc.pc += 4
            data.ccr.clearV
            buf = checkZ(mov, ArrayBuffer(data))
            buf = checkN(mov, buf)

          case 0x80 =>
            //MOV.W Reg,Abs [6B][8reg][abs][abs]
            val abs = rom.readWord(pc + 2)
            val mov = data.reg.getWord(op1)
            data.mem.setWord(mov, abs)
            data.pc.pc += 4
            data.ccr.clearV
            buf = checkZ(mov, ArrayBuffer(data))
            buf = checkN(mov, buf)
        }

      case 0x6D =>
        val op1 = rom.readByte(pc + 1)
        op1 & 0xF0 match {
          case 0x70 =>
            //POP.W Reg [6D][7Reg]
            val sp = data.reg.getLong(7)
            val pop = data.mem.getWord(sp)
            data.reg.setLong(sp + 2, 7)
            data.pc.pc += 2
            data.ccr.clearV
            pop.foreach { p =>
              val clone = data.clone
              clone.reg.setWord(p, op1)
              var buff = ArrayBuffer(clone)
              buff = checkZ(p, buff)
              buf ++= checkN(p, buff)
            }

          case 0xF0 =>
            //PUSH.W Reg [6D][FReg]
            val sp = data.reg.getLong(7)
            val push = data.reg.getWord(op1)
            data.reg.setLong(sp - 2, 7)
            data.pc.pc += 2
            data.ccr.clearV
            Main.extract(0 to 0xFFFF, sp).foreach { s =>
              val clone = data.clone
              data.mem.setWord(push, s - 2)
              var buff = ArrayBuffer(clone)
              buff = checkZ(push, buff)
              buf ++= checkN(push, buff)
            }
        }

      case 0x6E =>
        //MOV.B Disp,Reg [6E][0dregreg][disp][disp]
        val op1 = rom.readByte(pc + 1)
        op1 & 0x80 match {
          case 0x00 =>
            val disp = data.mem.getWord(pc + 2)
            val dreg = data.reg.getLong(op1 >> 4)
            val add = dreg + disp
            data.pc.pc += 4
            data.ccr.clearV
            val mov = data.mem.getByte(add)
            mov.foreach { m =>
              val clone = data.clone
              clone.reg.setByte(m, op1)
              var buff = ArrayBuffer(clone)
              buff = checkZ(m, buff)
              buf ++= checkN(m, buff)
            }
        }
    }
    buf
  }

  private def analyze7(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    rom.readByte(pc) match {
      case 0x79 =>
        val op1 = rom.readByte(pc + 1)
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV.W Imm,Reg [79][0reg][imm][imm]
            val imm = data.mem.getWord(pc + 2)
            data.reg.setWord(imm, op1)
            data.pc.pc += 4
            data.ccr.clearV
            buf = checkZ(imm, ArrayBuffer(data))
            buf = checkN(imm, buf)

          case 0x10 =>
            //ADD.W Imm,Reg [79][1reg][imm][imm]
            val imm = data.mem.getWord(pc + 2)
            val reg = data.reg.getWord(op1)
            val add = reg + imm
            data.reg.setWord(add, op1)
            data.pc.pc += 4
            buf = checkC(reg, imm, add, ArrayBuffer(data))
            buf = checkV(reg, imm, add, buf)
            buf = checkZ(reg, buf)
            buf = checkN(reg, buf)
            buf = checkH(reg, imm, add, buf)

          case 0x20 =>
            //CMP.W Imm,Reg [79][2reg][imm][imm]
            val imm = data.mem.getWord(pc + 2)
            val reg = data.reg.getWord(op1)
            val cmp = reg - imm
            data.pc.pc += 4
            buf = checkC(reg, imm.neg, cmp, ArrayBuffer(data))
            buf = checkV(reg, imm.neg, cmp, buf)
            buf = checkZ(cmp, buf)
            buf = checkN(cmp, buf)
            buf = checkH(reg, imm.neg, cmp, buf)
        }

      case 0x7A =>
        val op1 = rom.readByte(pc + 1)
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV,L Imm,Reg [7A][0reg][imm][imm][imm][imm]
            val imm = data.mem.getLong(pc + 2)
            data.reg.setLong(imm, op1)
            data.pc.pc += 6
            data.ccr.clearV
            buf = checkZ(imm, ArrayBuffer(data))
            buf = checkN(imm, buf)
        }

      case 0x7F =>
        val op2 = rom.readByte(pc + 2)
        op2 match {
          case 0x70 =>
            //BSET.B Imm,Abs [7F][Abs][70][Imm0]
            val abs = rom.readByte(pc + 1)
            val imm = (rom.readByte(pc + 3) >> 4) & 0x7
            val bset = data.mem.getByte(abs)
            data.pc.pc += 4
            data.mem.setByte(bset.bitset(imm), abs)
            buf += data
        }

    }
    buf
  }

  private def checkC(data1: CtxSymbol, data2: CtxSymbol, res: CtxSymbol, buf: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    val and =
      res.symbol.getSort.toString match {
        case "(_ BitVec 8)" => 0x80
        case "(_ BitVec 16)" => 0x8000
        case "(_ BitVec 32)" => 0x80000000
      }
    buf.foreach {
      b =>
        val bool1 = (data1 & and).eq(and) && (data2 & and).eq(and)
        val bool2 = ((data1 & and).eq(and) || (data2 & and).eq(and)) && ((res & and).eq(and))
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
    ans
  }

  private def checkV(data1: CtxSymbol, data2: CtxSymbol, res: CtxSymbol, buf: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach {
      b =>
        val bool1 = data1 >= 0 && data2 >= 0 && res < 0
        val bool2 = data1 < 0 && data2 < 0 && res >= 0
        val bool = (bool1 || bool2)
        val bc1 = b.clone
        bc1.ccr.setV
        bc1.path.set(bool.symbol)
        ans += bc1
        val bc2 = b.clone
        bc2.ccr.clearV
        bc2.path.set(bool.not.symbol)
        ans += bc2
    }
    ans
  }

  private def checkZ(data: CtxSymbol, buf: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach {
      b =>
        val eq = data.eq(0)
        val bc1 = b.clone
        bc1.ccr.setZ //data = 0
        bc1.path.set(eq.symbol)
        ans += bc1
        val bc2 = b.clone
        bc2.ccr.clearZ
        bc2.path.set(eq.not.symbol)
        ans += bc2
    }
    ans
  }

  private def checkN(data: CtxSymbol, buf: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach {
      b =>
        val neg = data < 0
        val bc1 = b.clone
        bc1.ccr.setN //data < 0
        bc1.path.set(neg.symbol)
        ans += bc1
        val bc2 = b.clone
        bc2.ccr.clearN
        bc2.path.set(neg.not.symbol)
        ans += bc2
    }
    ans
  }

  private def checkH(data1: CtxSymbol, data2: CtxSymbol, res: CtxSymbol, buf: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    val and =
      res.symbol.getSort.toString match {
        case "(_ BitVec 8)" => 0x08
        case "(_ BitVec 16)" => 0x0800
        case "(_ BitVec 32)" => 0x00800000
      }
    buf.foreach {
      b =>
        val bool1 = (data1 & and).eq(and) && (data2 & and).eq(and)
        val bool2 = ((data1 & and).eq(and) || (data2 & and).eq(and)) && ((res & and).eq(and))
        val bool = (bool1 || bool2)
        val bc1 = b.clone
        bc1.ccr.setH
        bc1.path.set(bool.symbol)
        ans += bc1
        val bc2 = b.clone
        bc2.ccr.clearH
        bc2.path.set(bool.not.symbol)
        ans += bc2
    }
    ans
  }

}
