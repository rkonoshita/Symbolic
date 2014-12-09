package main

import data.DataSet
import data.register.ROM
import symbol.{MySymbol, CtxSymbol, IntSymbol}
import z3.scala.Z3Context

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/17.
 */
class Decoder(c: Z3Context, r: ROM) {

  private val ctx = c
  private val rom = r

  def analyze(data: DataSet): ArrayBuffer[DataSet] = {
    decode(data.clone, data.pc.pc)
  }

  private def decode(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    val op0 = rom.getByte(pc)
    op0 & 0xF0 match {
      case 0x00 => buf ++= analyze0(data, pc)

      case 0x10 => buf ++= analyze1(data, pc)

      case 0x20 =>
        //MOV.B Abs,Reg [2reg][abs]
        val abs = rom.getByte(pc + 1) | 0xFFFF00
        val mov = data.mem.getByte(abs)
        data.reg.setByte(mov, op0)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        buf = checkZ(mov, ArrayBuffer(data))
        buf = checkN(mov, buf, 8)

      case 0x30 =>
        //MOV.B Reg,Abs [3reg][abs]
        val abs = rom.getByte(pc + 1)
        val mov = data.reg.getByte(op0)
        data.mem.setByte(mov, abs)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        buf = checkZ(mov, ArrayBuffer(data))
        buf = checkN(mov, buf, 8)

      case 0x40 => buf ++= analyze4(data, pc)

      case 0x50 => buf ++= analyze5(data, pc)

      case 0x60 => buf ++= analyze6(data, pc)

      case 0x70 => buf ++= analyze7(data, pc)

      case 0x80 =>
        //ADD.B Imm,Reg [8Reg][Imm]
        val imm = new IntSymbol(rom.getByte(pc + 1))
        val reg = data.reg.getByte(op0)
        val add = reg + imm
        data.reg.setByte(add, op0)
        data.pc.setPc(pc + 2)
        buf = checkC(reg, imm, add, ArrayBuffer(data), 8)
        buf = checkV(reg, imm, add, buf, 8)
        buf = checkZ(add, buf)
        buf = checkN(add, buf, 8)
        buf = checkH(reg, imm, add, buf, 8)

      case 0xA0 =>
        //CMP.B Imm,Reg [Areg][imm]
        val imm = new IntSymbol(rom.getByte(pc + 1))
        val reg = data.reg.getByte(op0)
        val cmp = reg - imm
        data.pc.setPc(pc + 2)
        buf = checkC(reg, imm.neg, cmp, ArrayBuffer(data), 8)
        buf = checkV(reg, imm.neg, cmp, buf, 8)
        buf = checkZ(cmp, buf)
        buf = checkN(cmp, buf, 8)
        buf = checkH(reg, imm.neg, cmp, buf, 8)

      case 0xE0 =>
        //AND.B Imm,Reg [Ereg][Imm]
        val imm = new IntSymbol(rom.getByte(pc + 1))
        val and = data.reg.getByte(op0) & imm
        data.reg.setByte(and, op0)
        data.ccr.clearV
        data.pc.setPc(pc + 2)
        buf = checkZ(imm, ArrayBuffer(data))
        buf = checkN(imm, buf, 8)

      case 0xF0 =>
        //MOV.B Imm,Reg [Freg][Imm]
        val imm = new IntSymbol(rom.getByte(pc + 1))
        data.reg.setByte(imm, op0)
        data.ccr.clearV
        data.pc.setPc(pc + 2)
        buf = checkZ(imm, ArrayBuffer(data))
        buf = checkN(imm, buf, 8)
    }
    buf
  }

  private def analyze0(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x04 =>
        //ORC.B Imm,Ccr [04][imm]
        val imm = rom.getByte(pc + 1)
        data.ccr.ccr = data.ccr.ccr | imm
        data.pc.setPc(pc + 2)
        buf += data

      case 0x06 =>
        //ANDC.B Imm,Ccr [06][imm]
        val imm = rom.getByte(pc + 1)
        data.ccr.ccr = data.ccr.ccr & imm
        data.pc.setPc(pc + 2)
        buf += data

      case 0x0B =>
        val op1 = rom.getByte(pc + 1)
        op1 & 0xF0 match {
          case 0x50 =>
            //INC.W #1,Reg [0B][5reg]
            val reg = data.reg.getWord(op1)
            val inc = reg + 1
            data.reg.setWord(inc, op1)
            data.pc.setPc(pc + 2)
            buf = checkV(reg, new IntSymbol(1), inc, ArrayBuffer(data), 16)
            buf = checkZ(inc, buf)
            buf = checkN(inc, buf, 16)
        }

      case 0x0D =>
        //MOV.W RegA,RegB [0D][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4)
        data.reg.setWord(regA, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        buf = checkZ(regA, ArrayBuffer(data))
        buf = checkN(regA, buf, 16)
    }
    buf
  }

  private def analyze1(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x17 =>
        val op1 = rom.getByte(pc + 1)
        op1 & 0xF0 match {
          case 0x00 =>
            //NOT.B Reg [17][0reg]
            val reg = data.reg.getByte(op1)
            val not = reg.~
            data.reg.setByte(not, op1)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            buf = checkZ(not, ArrayBuffer(data))
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
            buf = checkZ(extu, ArrayBuffer(data))

        }
      case 0x18 =>
        //SUB.B RegA,RegB [18][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val sub = regB - regA
        data.reg.setByte(sub, op1)
        data.pc.setPc(pc + 2)
        buf = checkC(regB, regA.neg, sub, ArrayBuffer(data), 8)
        buf = checkV(regB, regA.neg, sub, buf, 8)
        buf = checkZ(sub, buf)
        buf = checkN(sub, buf, 8)
        buf = checkH(regB, regA.neg, sub, buf, 8)

      case 0x19 =>
        //SUB.W RegA,RegB [19][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val sub = regB - regA
        data.reg.setWord(sub, op1)
        data.pc.setPc(pc + 2)
        buf = checkC(regB, regA.neg, sub, ArrayBuffer(data), 16)
        buf = checkV(regB, regA.neg, sub, buf, 16)
        buf = checkZ(sub, buf)
        buf = checkN(sub, buf, 16)
        buf = checkH(regB, regA.neg, sub, buf, 16)

      case 0x1D =>
        //CMP.W RegA,RegB [1D][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val sub = regB - regA
        data.pc.setPc(pc + 2)
        buf = checkC(regB, regA.neg, sub, ArrayBuffer(data), 16)
        buf = checkV(regB, regA.neg, sub, buf, 16)
        buf = checkZ(sub, buf)
        buf = checkN(sub, buf, 16)
        buf = checkH(regB, regA.neg, sub, buf, 16)
    }
    buf
  }

  private def analyze4(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    val op0 = rom.getByte(pc)
    val op1 = rom.getByte(pc + 1)
    val disp =
      if ((op1 & 0x80) == 0x80) op1 | 0xFF00
      else op1
    op0 match {
      //Bcc [4X][disp]
      case 0x40 =>
        //BRA true
        data.pc.setPc(pc + disp)
        buf += data
      case 0x42 =>
        //BHI C|V = 0
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            val z = (ccr & 0x04).eq(0x04)
            if (!(c | z)) data.pc.setPc(pc + disp)
            else data.pc.setPc(pc + 2)
            buf += data
          case ccr: CtxSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            val z = (ccr & 0x04).eq(0x04)
            val or = ctx.mkOr(c.symbol, z.symbol)
            val dc1 = data.clone
            dc1.pc.setPc(pc + 2)
            dc1.path.set(or)
            buf += dc1
            val dc2 = data.clone
            dc2.pc.setPc(pc + disp)
            dc2.path.set(ctx.mkNot(or))
            buf += dc2
        }
        data.pc.pc = pc + disp
      case 0x45 =>
        //BLO C = 1
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            if (c) data.pc.setPc(pc + disp)
            else data.pc.setPc(pc + 2)
          case ccr: CtxSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            val dc1 = data.clone
            dc1.pc.setPc(pc + disp)
            dc1.path.set(c.symbol)
            buf += dc1
            val dc2 = data.clone
            dc2.pc.setPc(pc + 2)
            dc1.path.set(c.not.symbol)
            buf += dc2
        }
      case 0x4D =>
        //BLT N 排他的論理和 V = 1
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val n = (ccr & 0x08).eq(0x08)
            val v = (ccr & 0x02).eq(0x02)
            if (n ^ v) data.pc.setPc(pc + disp)
            else data.pc.setPc(pc + 2)
          case ccr: CtxSymbol =>
            val n = (ccr & 0x08).eq(0x08)
            val v = (ccr & 0x02).eq(0x02)
            val xor = ctx.mkXor(n.symbol, v.symbol)
            val dc1 = data.clone
            dc1.pc.setPc(pc + disp)
            dc1.path.set(xor)
            buf += dc1
            val dc2 = data.clone
            dc2.pc.setPc(pc + 2)
            dc2.path.set(ctx.mkNot(xor))
            buf += dc2
        }
    }
    buf
  }

  private def analyze5(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x54 =>
        //RTS [54][70]
        val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
        val ret = data.mem.getWord(sp.symbol)
        data.reg.setLong(sp + 2, 7)
        ret match {
          case r: IntSymbol =>
            data.pc.setPc(r.symbol)
            buf += data
          case r: CtxSymbol =>
            Main.extract(0 to 0xFFFF, r).foreach { p =>
              val clone = data.clone
              clone.pc.setPc(p)
              clone.path.set(r.eq(p).symbol)
              buf += clone
            }
        }

      case 0x56 =>
        //RTE [56][70]
        val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
        val ret = data.mem.getLong(sp.symbol)
        ret match {
          case r: IntSymbol =>
            data.pc.setPc((r & 0xFFFF).symbol)
            data.ccr.ccr = (r >> 24) & 0xFF
            buf += data
          case r: CtxSymbol =>
            val setpc = r.extract(15, 0)
            data.ccr.ccr = r.extract(31, 24)
            Main.extract(0 to 0xFFFF, setpc).foreach { p =>
              val clone = data.clone
              clone.pc.setPc(p)
              clone.path.set(setpc.eq(p).symbol)
              buf += clone
            }
        }

      case 0x59 =>
        //JMP IndReg [59][reg0]
        val op1 = rom.getByte(pc + 1)
        val abs = data.reg.getLong(op1 >> 4)
        abs match {
          case a: IntSymbol =>
            data.pc.setPc(a.symbol)
            buf += data
          case a: CtxSymbol =>
            Main.extract(0 to 0xFFFF, a.extract(15, 0)).foreach { p =>
              val clone = data.clone
              data.pc.setPc(p)
              clone.path.set(a.extract(15, 0).eq(p).symbol)
              buf += clone
            }
        }

      case 0x5E =>
        //JSR Abs:24 [5E][Abs][Abs][Abs]
        val abs = rom.getLong(pc) & 0xFFFFFF
        val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
        val stock = data.pc.pc + 4
        data.mem.setWord(new IntSymbol(stock), (sp - 2).symbol)
        data.reg.setLong(sp - 2, 7)
        data.pc.setPc(abs)
        buf += data
    }
    buf
  }

  private def analyze6(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x68 =>
        val op1 = rom.getByte(pc + 1)
        op1 & 0x80 match {
          case 0x00 =>
            //MOV.B IndReg,Reg [68][0indreg reg]
            val abs = data.reg.getLong(op1 >> 4)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            abs match {
              case a: IntSymbol =>
                val mov = data.mem.getByte(a.symbol)
                data.reg.setByte(mov, op1)
                buf = checkZ(mov, ArrayBuffer(data))
                buf = checkN(mov, buf, 8)
              case a: CtxSymbol =>
                Main.extract(0 to 0xFFFF, a.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  val mov = clone.mem.getByte(p)
                  clone.reg.setByte(mov, op1)
                  clone.path.set(a.extract(15, 0).eq(p).symbol)
                  var buff = ArrayBuffer(clone)
                  buff = checkZ(mov, buff)
                  buf ++= checkN(mov, buff, 8)
                }
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
                buf = checkZ(mov, ArrayBuffer(data))
                buf = checkN(mov, buf, 8)
              case a: CtxSymbol =>
                Main.extract(0 to 0xFFFF, a.extract(15, 0).symbol).foreach { p =>
                  val clone = data.clone
                  clone.mem.setByte(mov, p)
                  clone.path.set(a.extract(15, 0).eq(p).symbol)
                  var buff = ArrayBuffer(clone)
                  buff = checkZ(mov, buff)
                  buf ++= checkN(mov, buff, 8)
                }
            }
        }

      case 0x6A =>
        val op1 = rom.getByte(pc + 1)
        op1 & 0xF0 match {
          case 0x80 =>
            //MOV.B Reg,Abs [6A][8reg][abs][abs]
            val mov = data.reg.getByte(op1)
            val abs = rom.getWord(pc + 2)
            data.mem.setByte(mov, abs)
            data.pc.setPc(pc + 4)
            data.ccr.clearV
            buf = checkZ(mov, ArrayBuffer(data))
            buf = checkN(mov, buf, 8)
        }

      case 0x6B =>
        val op1 = rom.getByte(pc + 1)
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV.W Abs,Reg [6B][0Reg][abs][abs]
            val abs = rom.getWord(pc + 2)
            val mov = data.mem.getWord(abs)
            data.reg.setWord(mov, op1)
            data.pc.setPc(pc + 4)
            data.ccr.clearV
            buf = checkZ(mov, ArrayBuffer(data))
            buf = checkN(mov, buf, 16)

          case 0x80 =>
            //MOV.W Reg,Abs [6B][8reg][abs][abs]
            val abs = rom.getWord(pc + 2)
            val mov = data.reg.getWord(op1)
            data.mem.setWord(mov, abs)
            data.pc.setPc(pc + 4)
            data.ccr.clearV
            buf = checkZ(mov, ArrayBuffer(data))
            buf = checkN(mov, buf, 16)
        }

      case 0x6D =>
        val op1 = rom.getByte(pc + 1)
        op1 & 0xF0 match {
          case 0x70 =>
            //POP.W Reg [6D][7Reg]
            val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
            val pop = data.mem.getWord(sp.symbol)
            data.reg.setWord(pop, op1)
            data.reg.setLong(sp + 2, 7)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            buf = checkZ(pop, ArrayBuffer(data))
            buf = checkN(pop, buf, 16)

          case 0xF0 =>
            //PUSH.W Reg [6D][FReg]
            val push = data.reg.getWord(op1)
            val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
            data.mem.setWord(push, (sp - 2).symbol)
            data.reg.setLong(sp - 2, 7)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            buf = checkZ(push, ArrayBuffer(data))
            buf = checkN(push, buf, 16)
        }

      case 0x6E =>
        //MOV.B Disp,Reg [6E][dregreg][disp][disp]
        val op1 = rom.getByte(pc + 1)
        val disp = rom.getWord(pc + 2)
        val dreg = data.reg.getLong(op1 >> 4)
        data.pc.setPc(pc + 4)
        data.ccr.clearV
        dreg match {
          case d: IntSymbol =>
            val add = d + disp
            val mov = data.mem.getByte(add.symbol)
            data.reg.setByte(mov, op1)
            buf = checkZ(mov, ArrayBuffer(data))
            buf = checkN(mov, buf, 8)
          case d: CtxSymbol =>
            val add = d.extract(15, 0) + disp
            Main.extract(0 to 0xFFFF, add).foreach { a =>
              val clone = data.clone
              val mov = clone.mem.getByte(a)
              clone.reg.setByte(mov, op1)
              clone.path.set(add.eq(a).symbol)
              var buff = ArrayBuffer(clone)
              buff = checkZ(mov, buff)
              buf ++= checkN(mov, buff, 8)
            }
        }
    }
    buf
  }

  private def analyze7(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    var buf = new ArrayBuffer[DataSet]
    op0 match {
      case 0x79 =>
        val op1 = rom.getByte(pc + 1)
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV.W Imm,Reg [79][0reg][imm][imm]
            val imm = new IntSymbol(rom.getWord(pc + 2))
            data.reg.setWord(imm, op1)
            data.pc.setPc(pc + 4)
            data.ccr.clearV
            buf = checkZ(imm, ArrayBuffer(data))
            buf = checkN(imm, buf, 16)

          case 0x10 =>
            //ADD.W Imm,Reg [79][1reg][imm][imm]
            val imm = new IntSymbol(rom.getWord(pc + 2))
            val reg = data.reg.getWord(op1)
            val add = reg + imm
            data.reg.setWord(add, op1)
            data.pc.setPc(pc + 4)
            buf = checkC(reg, imm, add, ArrayBuffer(data), 16)
            buf = checkV(reg, imm, add, buf, 16)
            buf = checkZ(reg, buf)
            buf = checkN(reg, buf, 16)
            buf = checkH(reg, imm, add, buf, 16)

          case 0x20 =>
            //CMP.W Imm,Reg [79][2reg][imm][imm]
            val imm = new IntSymbol(rom.getWord(pc + 2))
            val reg = data.reg.getWord(op1)
            val cmp = reg - imm
            data.pc.setPc(pc + 4)
            buf = checkC(reg, imm.neg, cmp, ArrayBuffer(data), 16)
            buf = checkV(reg, imm.neg, cmp, buf, 16)
            buf = checkZ(cmp, buf)
            buf = checkN(cmp, buf, 16)
            buf = checkH(reg, imm.neg, cmp, buf, 16)
        }

      case 0x7A =>
        val op1 = rom.getByte(pc + 1)
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV,L Imm,Reg [7A][0reg][imm][imm][imm][imm]
            val imm = new IntSymbol(rom.getLong(pc + 2))
            data.reg.setLong(imm, op1)
            data.pc.setPc(pc + 6)
            data.ccr.clearV
            buf = checkZ(imm, ArrayBuffer(data))
            buf = checkN(imm, buf, 32)
        }

      case 0x7F =>
        val op2 = rom.getByte(pc + 2)
        op2 match {
          case 0x70 =>
            //BSET.B Imm,Abs [7F][Abs][70][Imm0]
            val abs = rom.getByte(pc + 1)
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
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and)
          val bool2 = ((d1 & and).eq(and) || (d2 & and).eq(and)) && ((r & and).eq(0))
          val bool = bool1 | bool2
          val clone = b.clone
          if (bool) clone.ccr.setC else clone.ccr.clearC
          ans += clone
        case _ =>
          val bool1 = (data1 & and).eq(new CtxSymbol(ctx, and, size)) && (data2 & and).eq(new CtxSymbol(ctx, and, size))
          val bool2 = ((data1 & and).eq(new CtxSymbol(ctx, and, size)) || (data2 & and).eq(new CtxSymbol(ctx, and, size))) && ((res & and).eq(new CtxSymbol(ctx, 0, size)))
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
    buf.foreach { b =>
      (data1, data2, res) match {
        case (d1: IntSymbol, d2: IntSymbol, r: IntSymbol) =>
          val and =
            size match {
              case 8 => 0x80
              case 16 => 0x8000
              case 32 => 0x80000000
            }
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and) && (r & and).eq(0)
          val bool2 = (d1 & and).eq(0) && (d2 & and).eq(0) && (r & and).eq(and)
          val bool = bool1 | bool2
          val clone = b.clone
          if (bool) clone.ccr.setV else clone.ccr.clearV
          ans += clone
        case _ =>
          val bool1 = data1 >= new CtxSymbol(ctx, 0, size) && data2 >= new CtxSymbol(ctx, 0, size) && res < new CtxSymbol(ctx, 0, size)
          val bool2 = data1 < new CtxSymbol(ctx, 0, size) && data2 < new CtxSymbol(ctx, 0, size) && res >= new CtxSymbol(ctx, 0, size)
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

  private def checkZ(data: MySymbol, buf: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b =>
      data match {
        case d: IntSymbol =>
          val clone = b.clone
          if (d == 0) clone.ccr.setZ else clone.ccr.clearZ
          ans += clone
        case d: CtxSymbol =>
          val eq = d.eq(0)
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
    buf.foreach { b =>
      data match {
        case d: IntSymbol =>
          val and =
            size match {
              case 8 => 0x80
              case 16 => 0x8000
              case 32 => 0x80000000
            }
          val clone = b.clone
          if ((d & and).eq(and)) clone.ccr.setN else clone.ccr.clearN
          ans += clone
        case d: CtxSymbol =>
          val neg = d < 0
          val bc1 = b.clone
          bc1.ccr.setN //data < 0
          bc1.path.set(neg.symbol)
          ans += bc1
          val bc2 = b.clone
          bc2.ccr.clearN
          bc2.path.set(neg.not.symbol)
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
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and)
          val bool2 = ((d1 & and).eq(and) || (d2 & and).eq(and)) && ((r & and).eq(0))
          val bool = bool1 | bool2
          val clone = b.clone
          if (bool) clone.ccr.setH else clone.ccr.clearH
          ans += clone
        case _ =>
          val bool1 = (data1 & and).eq(new CtxSymbol(ctx, and, size)) && (data2 & and).eq(new CtxSymbol(ctx, and, size))
          val bool2 = ((data1 & and).eq(new CtxSymbol(ctx, and, size)) || (data2 & and).eq(new CtxSymbol(ctx, and, size))) && ((res & and).eq(new CtxSymbol(ctx, 0, size)))
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
