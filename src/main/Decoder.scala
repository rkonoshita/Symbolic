package main

import data.DataSet
import data.register.ROM
import parser.Data
import symbol.{MySymbol, CtxSymbol, IntSymbol}
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/17.
 */
class Decoder {

  private val ctx = Main.ctx
  private val rom = Main.rom

  def analyze(data: DataSet): ArrayBuffer[DataSet] = analyze(data.clone, data.pc.pc)

  private def analyze(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    op0 & 0x80 match {
      case 0x00 =>
        op0 & 0xF0 match {
          case 0x00 => analyze0(data, pc)
          case 0x10 => analyze1(data, pc)
          case 0x20 =>
            //MOV.B Abs,Reg [2reg][abs]
            val mov = data.mem.getByte(rom.getByte(pc + 1) | 0xFFFF00)
            data.reg.setByte(mov, op0)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            check2(mov, 8, data)

          case 0x30 =>
            //MOV.B Reg,Abs [3reg][abs]
            val mov = data.reg.getByte(op0)
            data.mem.setByte(mov, rom.getByte(pc + 1) | 0xFFFF00)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            check2(mov, 8, data)

          case 0x40 => analyze4(data, pc)
          case 0x50 => analyze5(data, pc)
          case 0x60 => analyze6(data, pc)
          case 0x70 => analyze7(data, pc)
        }

      case 0x80 => analyze8(data, pc)
    }
  }

  private def analyze8(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    val imm = new IntSymbol(rom.getByte(pc + 1))
    data.pc.setPc(pc + 2)
    op0 & 0xF0 match {
      case 0x80 =>
        //ADD.B Imm,Reg [8Reg][Imm]
        val reg = data.reg.getByte(op0)
        val add = reg + imm
        data.reg.setByte(add, op0)
        check5(reg, imm, add, 8, data)

      case 0x90 =>
        //ADDX.B Imm,Reg [9reg][imm]
        val reg = data.reg.getByte(op0)
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val immx = imm + (if ((ccr & 0x01).eq(0x01)) 1 else 0)
            val add = reg + immx
            data.reg.setByte(add, op0)
            check5(reg, immx, add, 8, data)
          case ccr: CtxSymbol =>
            val immx = imm + ccr.extract(1, 0).zextend(7)
            val add = reg + immx
            data.reg.setByte(add, op0)
            check5(reg, immx, add, 8, data)
        }

      case 0xA0 =>
        //CMP.B Imm,Reg [Areg][imm]
        val reg = data.reg.getByte(op0)
        val cmp = reg - imm
        check5(reg, imm.neg, cmp, 8, data)

      case 0xB0 =>
        //SUBX.B Imm,Reg [Breg][imm]
        val reg = data.reg.getByte(op0)
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val immx = imm + (if ((ccr & 0x01).eq(0x01)) 1 else 0)
            val sub = reg - immx
            data.reg.setByte(sub, op0)
            check5(reg, immx.neg, sub, 8, data)
          case ccr: CtxSymbol =>
            val immx = imm + ccr.extract(1, 0).zextend(7)
            val sub = reg - immx
            data.reg.setByte(sub, op0)
            check5(reg, immx.neg, sub, 8, data)
        }

      case 0xC0 =>
        //OR.B Imm,Reg [Creg][Imm]
        val or = data.reg.getByte(op0) | imm
        data.reg.setByte(or, op0)
        data.ccr.clearV
        check2(or, 8, data)

      case 0xD0 =>
        //XOR.B Imm,Reg [Dreg][Imm]
        val xor = data.reg.getByte(op0) ^ imm
        data.reg.setByte(xor, op0)
        data.ccr.clearV
        check2(xor, 8, data)

      case 0xE0 =>
        //AND.B Imm,Reg [Ereg][Imm]
        val and = data.reg.getByte(op0) & imm
        data.reg.setByte(and, op0)
        data.ccr.clearV
        check2(and, 8, data)

      case 0xF0 =>
        //MOV.B Imm,Reg [Freg][Imm]
        data.reg.setByte(imm, op0)
        data.ccr.clearV
        check2(imm, 8, data)
    }
  }

  private def analyze0(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    val op1 = rom.getByte(pc + 1)
    op0 match {
      case 0x00 =>
        //NOP [00][00]
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x01 => analyze01(data, pc)

      case 0x02 =>
        //STC.B CCR,Reg [02][0reg]
        data.reg.setByte(data.ccr.ccr, op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x03 =>
        //LDC.B Reg,CCR [03][0reg]
        data.ccr.ccr = data.reg.getByte(op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x04 =>
        //ORC.B Imm,Ccr [04][imm]
        data.ccr.ccr = data.ccr.ccr | op1
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x05 =>
        //ANDC.B Imm,Ccr [06][imm]
        data.ccr.ccr = data.ccr.ccr ^ op1
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x06 =>
        //ANDC.B Imm,Ccr [06][imm]
        data.ccr.ccr = data.ccr.ccr & op1
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x07 =>
        //LDC.B Reg,CCR [03][imm]
        data.ccr.ccr = new IntSymbol(op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x08 =>
        //ADD.B RegA,RegB [08][regAregB]
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val add = regB + regA
        data.reg.setByte(add, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA, add, 8, data)

      case 0x09 =>
        //ADD.W RegA,RegB [09][regAregB]
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val add = regB + regA
        data.reg.setWord(add, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA, add, 16, data)

      case 0x0A => analyze0A(data, pc)
      case 0x0B => analyze0B(data, pc)

      case 0x0C =>
        //MOV.B RegA,RegB [0C][regAregB]
        val mov = data.reg.getByte(op1 >> 4)
        data.reg.setByte(mov, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(mov, 8, data)

      case 0x0D =>
        //MOV.W RegA,RegB [0D][regAregB]
        val mov = data.reg.getWord(op1 >> 4)
        data.reg.setWord(mov, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(mov, 16, data)

      case 0x0E =>
        //ADDX.B RegA,RegB [0E][regAregB]
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        data.pc.setPc(pc + 2)
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val regAx = regA + (if ((ccr & 0x01).eq(0x01)) 1 else 0)
            val add = regB + regAx
            data.reg.setByte(add, op1)
            check5(regB, regAx, add, 8, data)
          case ccr: CtxSymbol =>
            val regAx = regA + ccr.extract(1, 0).zextend(7)
            val add = regB + regAx
            data.reg.setByte(add, op0)
            check5(regB, regAx, add, 8, data)
        }

      case 0x0F => analyze0F(data, pc)
    }
  }

  private def analyze01(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc + 1) & 0xF0 match {
      case 0x00 => analyze0100(data, pc)
      case 0x04 => analyze0140(data, pc)
      //case 0x80 => SLEEP
      case 0xC0 => analyze01C(data, pc)
      case 0xD0 => analyze01D(data, pc)
      case 0xF0 => analyze01F06(data, pc)

    }
  }

  private def analyze0100(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc + 2) match {
      case 0x69 => analyze010069(data, pc)
      case 0x6B => analyze01006B(data, pc)
      case 0x6D => analyze01006D(data, pc)
      case 0x6F => analyze01006F(data, pc)
      case 0x78 => analyze010078(data, pc)
    }
  }

  private def analyze010069(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    data.pc.setPc(pc + 4)
    data.ccr.clearV
    op3 & 0x80 match {
      case 0x00 =>
        //MOV.L Indreg,Reg [01][00][69][indreg reg]
        data.reg.getLong(op3 >> 4) match {
          case ind: IntSymbol =>
            val mov = data.mem.getLong(ind.symbol)
            data.reg.setLong(mov, op3)
            check2(mov, 32, data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              val mov = clone.mem.getLong(p)
              clone.reg.setLong(mov, op3)
              buf ++= check2(mov, 32, clone)
            }
            buf
        }

      case 0x80 =>
        //MOV.L Reg,IndReg [01][00][69][1indreg reg]
        val mov = data.reg.getLong(op3)
        data.reg.getLong(op3 >> 4) match {
          case ind: IntSymbol =>
            data.mem.setLong(mov, ind.symbol)
            check2(mov, 32, data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              clone.mem.setLong(mov, p)
              buf ++= check2(mov, 32, clone)
            }
            buf
        }
    }
  }

  private def analyze01006B(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    data.ccr.clearV
    op3 & 0x80 match {
      case 0x00 =>
        val abs = op3 & 0xF0 match {
          case 0x00 =>
            //MOV.L Abs,Reg [01][00][6B][0reg][abs][abs]
            data.pc.setPc(pc + 6)
            rom.getWord(pc + 4)

          case 0x20 =>
            //MOV.L Abs,Reg [01][00][6B][2reg][00][abs][abs][abs]
            data.pc.setPc(pc + 8)
            rom.getLong(pc + 4)
        }
        val mov = data.mem.getLong(abs)
        data.reg.setLong(mov, op3)
        check2(mov, 32, data)

      case 0x80 =>
        val mov = data.reg.getLong(op3)
        val abs = op3 & 0xF0 match {
          case 0x80 =>
            //MOV.L Reg,Abs [01][00][6B][8reg][abs][abs]
            data.pc.setPc(pc + 6)
            rom.getWord(pc + 4)

          case 0xA0 =>
            //MOV.L Reg,Abs [01][00][6B][Areg][00][abs][abs][abs]
            data.pc.setPc(pc + 8)
            rom.getLong(pc + 4)
        }
        data.mem.setLong(mov, abs)
        check2(mov, 32, data)
    }
  }

  private def analyze01006D(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    data.pc.setPc(pc + 4)
    data.ccr.clearV
    op3 & 0x80 match {
      case 0x00 =>
        //MOV.L @Reg+,Reg [01][00][6D][pregreg]
        data.reg.getLong(op3 >> 4) match {
          case ind: IntSymbol =>
            val mov = data.mem.getLong(ind.symbol)
            data.reg.setLong(mov, op3)
            data.reg.setLong(ind + 4, op3 >> 4)
            check2(mov, 32, data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              val mov = clone.mem.getLong(p)
              clone.reg.setLong(mov, op3)
              clone.reg.setLong(ind + 4, op3)
              buf ++= check2(mov, 32, clone)
            }
            buf
        }

      case 0x80 =>
        //MOV.L Reg,@-Reg [01][00][6D][1pregreg]
        val mov = data.reg.getLong(op3)
        data.reg.getLong(op3 >> 4) - 4 match {
          case ind: IntSymbol =>
            data.mem.setLong(mov, ind.symbol)
            data.reg.setLong(ind, op3 >> 4)
            check2(mov, 32, data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              clone.mem.setLong(mov, p)
              clone.reg.setLong(ind, op3 >> 4)
              buf ++= check2(mov, 32, clone)
            }
            buf
        }
    }
  }

  private def analyze01006F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val disp = rom.getWord(pc + 2)
    data.ccr.clearV
    data.pc.setPc(pc + 6)
    op3 & 0x80 match {
      case 0x00 =>
        //MOV.L DIsp16,Reg [01][00][6F][dregreg][disp][disp]
        data.reg.getLong(op3 >> 4) + disp match {
          case d: IntSymbol =>
            val mov = data.mem.getLong(d.symbol)
            data.reg.setLong(mov, op3)
            check2(mov, 32, data)
          case d: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
              val clone = pathClone(d.extract(15, 0).eq(p), data)
              val mov = clone.mem.getLong(p)
              clone.reg.setLong(mov, op3)
              buf ++= check2(mov, 32, clone)
            }
            buf
        }

      case 0x80 =>
        val mov = data.reg.getLong(op3)
        //MOV.L Reg,Disp16 [01][00][6F][1dregreg][disp][disp]
        data.reg.getLong(op3 >> 4) + disp match {
          case d: IntSymbol =>
            data.mem.setLong(mov, d.symbol)
            check2(mov, 32, data)
          case d: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
              val clone = pathClone(d.extract(15, 0).eq(p), data)
              clone.mem.setLong(mov, p)
              buf ++= check2(mov, 32, clone)
            }
            buf
        }
    }
  }

  private def analyze010078(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val disp = rom.getLong(pc + 6)
    val op3 = rom.getByte(pc + 3)
    val op5 = rom.getByte(pc + 5)
    data.pc.setPc(pc + 10)
    data.ccr.clearV
    op3 & 0x80 match {
      case 0x00 =>
        //MOV.L Disp24,Reg [01][00][78][dreg0][6B][2reg][00][disp][disp][disp]
        data.reg.getLong(op3 >> 4) + disp match {
          case d: IntSymbol =>
            val mov = data.mem.getLong(d.symbol)
            data.reg.setLong(mov, op5)
            check2(mov, 32, data)
          case d: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
              val clone = pathClone(d.extract(15, 0).eq(p), data)
              val mov = clone.mem.getLong(p)
              clone.reg.setLong(mov, op5)
              buf ++= check2(mov, 32, clone)
            }
            buf
        }

      case 0x80 =>
        val mov = data.reg.getLong(op5)
        //MOV.L Reg,Disp24 [01][00][78][1dreg0][6B][Areg][00][disp][disp][disp]
        data.reg.getLong(op3 >> 4) match {
          case d: IntSymbol =>
            data.mem.setLong(mov, d.symbol)
            check2(mov, 32, data)
          case d: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
              val clone = pathClone(d.extract(15, 0).eq(p), data)
              clone.mem.setLong(mov, p)
              buf ++= check2(mov, 32, clone)
            }
            buf
        }
    }
  }

  private def analyze0140(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc + 2) match {
      case 0x69 => analyze014069(data, pc)
      case 0x6B => analyze01406B(data, pc)
      case 0x6D => analyze01406D(data, pc)
      case 0x6F => analyze01406F(data, pc)
      case 0x78 => analyze014078(data, pc)
    }
  }

  private def analyze014069(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    data.pc.setPc(pc + 4)
    op3 & 0x80 match {
      case 0x00 =>
        //LDC.W IndReg,CCR [01][40][69][reg0]
        data.reg.getLong(op3 >> 4) match {
          case ind: IntSymbol =>
            //ワードサイズの上位ビットだけつかうならバイトサイズでいいじゃん
            data.ccr.ccr = data.mem.getByte(ind.symbol)
            ArrayBuffer(data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              clone.ccr.ccr = clone.mem.getByte(p)
              buf += clone
            }
            buf
        }

      case 0x80 =>
        //STC.W CCR,IndReg [01][40][69][1reg0]
        data.reg.getLong(op3 >> 4) match {
          case ind: IntSymbol =>
            //ワードサイズで転送って、残りの8ビットは一体何なんだ(気になるなら8bitの記号を使う)
            data.mem.setByte(data.ccr.ccr, ind.symbol)
            ArrayBuffer(data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              clone.mem.setByte(data.ccr.ccr, p)
              buf += clone
            }
            buf
        }
    }
  }

  private def analyze01406B(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    op3 & 0x80 match {
      case 0x00 =>
        val abs = op3 & 0xF0 match {
          case 0x00 =>
            //LDC.W Abs,CCR [01][40][6B][00][abs][abs]
            data.pc.setPc(pc + 6)
            rom.getWord(pc + 4)

          case 0x20 =>
            //LDC.W Abs,CCR [01][40][6B][20][00][abs][abs][abs]
            data.pc.setPc(pc + 8)
            rom.getLong(pc + 4)
        }
        data.ccr.ccr = data.mem.getByte(abs)

      case 0x80 =>
        val mov = data.reg.getLong(op3)
        val abs = op3 & 0xF0 match {
          case 0x80 =>
            //STC.W CCR,Abs [01][40][6B][80][abs][abs]
            data.pc.setPc(pc + 6)
            rom.getWord(pc + 4)

          case 0xA0 =>
            //STC.W CCR,Abs [01][40][6B][A0][00][abs][abs][abs]
            data.pc.setPc(pc + 8)
            rom.getLong(pc + 4)
        }
        data.mem.setByte(data.ccr.ccr, abs)
    }
    ArrayBuffer(data)
  }

  private def analyze01406D(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    data.pc.setPc(pc + 4)
    op3 & 0x80 match {
      case 0x00 =>
        //LDC.W @Reg+,CCR [01][40][6D][preg0]
        data.reg.getLong(op3 >> 4) match {
          case ind: IntSymbol =>
            data.ccr.ccr = data.mem.getByte(ind.symbol)
            data.reg.setLong(ind + 4, op3 >> 4)
            ArrayBuffer(data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              clone.ccr.ccr = clone.mem.getLong(p)
              clone.reg.setLong(ind + 4, op3)
              buf += clone
            }
            buf
        }

      case 0x80 =>
        //STC.W CCR,@-Reg [01][40][6D][1preg0]
        data.reg.getLong(op3 >> 4) - 4 match {
          case ind: IntSymbol =>
            data.mem.setByte(data.ccr.ccr, ind.symbol)
            data.reg.setLong(ind, op3 >> 4)
            ArrayBuffer(data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              clone.mem.setByte(data.ccr.ccr, p)
              clone.reg.setLong(ind, op3 >> 4)
              buf += clone
            }
            buf
        }
    }
  }

  private def analyze01406F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val disp = rom.getWord(pc + 2)
    data.pc.setPc(pc + 6)
    op3 & 0x80 match {
      case 0x00 =>
        //LDC.W DIsp16,CCR [01][40][6F][dreg0][disp][disp]
        data.reg.getLong(op3 >> 4) + disp match {
          case d: IntSymbol =>
            data.ccr.ccr = data.mem.getByte(d.symbol)
            ArrayBuffer(data)
          case d: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
              val clone = pathClone(d.extract(15, 0).eq(p), data)
              data.ccr.ccr = clone.mem.getByte(p)
              buf += clone
            }
            buf
        }

      case 0x80 =>
        //STC.W CCR,Disp16 [01][40][6F][1dreg0][disp][disp]
        data.reg.getLong(op3 >> 4) + disp match {
          case d: IntSymbol =>
            data.mem.setByte(data.ccr.ccr, d.symbol)
            ArrayBuffer(data)
          case d: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
              val clone = pathClone(d.extract(15, 0).eq(p), data)
              clone.mem.setByte(data.ccr.ccr, p)
              buf += clone
            }
            buf
        }
    }
  }

  private def analyze014078(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val disp = rom.getLong(pc + 6)
    data.pc.setPc(pc + 10)
    op3 & 0x80 match {
      case 0x00 =>
        //STC.W Disp24,CCR [01][40][78][dreg0][6B][20][00][disp][disp][disp]
        data.reg.getLong(op3 >> 4) + disp match {
          case d: IntSymbol =>
            data.ccr.ccr = data.mem.getByte(d.symbol)
            ArrayBuffer(data)
          case d: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
              val clone = pathClone(d.extract(15, 0).eq(p), data)
              data.ccr.ccr = clone.mem.getByte(p)
              buf += clone
            }
            buf
        }

      case 0x80 =>
        //MOV.L Reg,Disp24 [01][00][78][1dreg0][6B][Areg][00][disp][disp][disp]
        data.reg.getLong(op3 >> 4) + disp match {
          case d: IntSymbol =>
            data.mem.setByte(data.ccr.ccr, d.symbol)
            ArrayBuffer(data)
          case d: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
              val clone = pathClone(d.extract(15, 0).eq(p), data)
              clone.mem.setByte(data.ccr.ccr, p)
              buf += clone
            }
            buf
        }
    }
  }

  private def analyze01C(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    data.pc.setPc(pc + 4)
    rom.getByte(pc + 2) match {
      case 0x50 =>
        //MULXS.B regB,regW [01][C0][50][regBregW]
        val regW = data.reg.getWord(op3) match {
          case reg: IntSymbol => if ((reg & 0x80).eq(0x80)) reg | 0xFFFFFF00 else reg & 0x000000FF
          case reg: CtxSymbol => reg.extract(7, 0).sextend(8)
        }
        val regB = data.reg.getByte(op3 >> 4) match {
          case reg: IntSymbol => reg | (if ((reg & 0x80).eq(0x80)) 0xFFFFFF00 else 0)
          case reg: CtxSymbol => reg.sextend(8)
        }
        val mul = regW * regB
        data.reg.setWord(mul, op3)
        check2(mul, 16, data)

      case 0x52 =>
        //MULXS.W regW,regL [01][C0][50][regWregL]
        val regL = data.reg.getLong(op3) match {
          case reg: IntSymbol => if ((reg & 0x8000).eq(0x8000)) reg | 0xFFFF0000 else reg & 0x0000FFFF
          case reg: CtxSymbol => reg.extract(31, 16).sextend(16)
        }
        val regW = data.reg.getWord(op3 >> 4) match {
          case reg: IntSymbol => reg | (if ((reg & 0x8000).eq(0x8000)) 0xFFFF0000 else 0)
          case reg: CtxSymbol => reg.sextend(16)
        }
        val mul = regL * regW
        data.reg.setLong(mul, op3)
        check2(mul, 32, data)
    }
  }

  private def analyze01D(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    data.pc.setPc(pc + 4)
    rom.getByte(pc + 2) match {
      case 0x50 =>
        //DIVXS.B regB,regW [01][D0][50][regBregW]
        val regW = data.reg.getWord(op3)
        val regB = data.reg.getByte(op3 >> 4) match {
          case reg: IntSymbol => reg | (if ((reg & 0x80).eq(0x80)) 0xFFFFFF00 else 0)
          case reg: CtxSymbol => reg.sextend(8)
        }
        regW sdiv regB match {
          case div: IntSymbol =>
            val ans = regW srem regB match {
              case rem: IntSymbol => ((rem & 0xFF) << 8) | (div | 0xFF)
              case rem: CtxSymbol => rem.extract(7, 0) concat new CtxSymbol(div & 0xFF, 8)
            }
            data.reg.setWord(ans, op3)
            checkN(div & 0xFF, checkZ(regB, ArrayBuffer(data), 8), 8)
          case div: CtxSymbol =>
            val ans = regW srem regB match {
              case rem: IntSymbol => new CtxSymbol(rem & 0xFF, 8) concat div.extract(7, 0)
              case rem: CtxSymbol => rem.extract(7, 0) concat div.extract(7, 0)
            }
            data.reg.setWord(ans, op3)
            checkN(div.extract(7, 0), checkZ(regB, ArrayBuffer(data), 8), 8)
        }

      case 0x52 =>
        //DIVXS.W regW,regL [01][D0][52][regWregL]
        val regL = data.reg.getLong(op3)
        val regW = data.reg.getWord(op3 >> 4) match {
          case reg: IntSymbol => reg | (if ((reg & 0x8000).eq(0x8000)) 0xFFFF0000 else 0)
          case reg: CtxSymbol => reg.sextend(16)
        }
        regL sdiv regW match {
          case div: IntSymbol =>
            val ans = regL srem regW match {
              case rem: IntSymbol => ((rem & 0xFF) << 16) | (div | 0xFF)
              case rem: CtxSymbol => rem.extract(15, 0) concat new CtxSymbol(div & 0xFFFF, 8)
            }
            data.reg.setLong(ans, op3)
            checkN(div & 0xFF, checkZ(regW, ArrayBuffer(data), 16), 16)
          case div: CtxSymbol =>
            val ans = regL srem regW match {
              case rem: IntSymbol => new CtxSymbol(rem & 0xFFFF, 8) concat div.extract(15, 0)
              case rem: CtxSymbol => rem.extract(15, 0) concat div.extract(15, 0)
            }
            data.reg.setLong(ans, op3)
            checkN(div.extract(15, 0), checkZ(regW, ArrayBuffer(data), 16), 16)
        }
    }
  }

  private def analyze01F06(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 3)
    val regA = data.reg.getLong(op1 >> 4)
    val regB = data.reg.getLong(op1)
    data.pc.setPc(pc + 4)
    data.ccr.clearV
    val logic = rom.getByte(pc + 2) match {
      //OR.L RegA,RegB [01][F0][64][regAregB]
      case 0x64 => regB | regA

      //XOR.L RegA,RegB [01][F0][65][regAregB]
      case 0x65 => regB ^ regA

      //AND.L RegA,RegB [01][F0][66][regAregB]
      case 0x66 => regB & regA
    }
    check2(logic, 32, data)
  }

  private def analyze0A(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    op1 & 0x80 match {
      case 0x00 =>
        op1 & 0xF0 match {
          case 0x00 =>
            //INC.B Reg [0A][0reg]
            val reg = data.reg.getByte(op1)
            val inc = reg + 1
            data.reg.setByte(inc, op1)
            data.pc.setPc(pc + 2)
            check3(reg, new IntSymbol(1), inc, 8, data)
        }

      case 0x80 =>
        //ADD.L RegA,RegB [0A][1regA regB]
        val regA = data.reg.getLong(op1 >> 4)
        val regB = data.reg.getLong(op1)
        val add = regB + regA
        data.reg.setLong(add, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA, add, 32, data)
    }
  }

  private def analyze0B(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.pc.setPc(pc + 2)
    op1 & 0xF0 match {
      case 0x00 =>
        //ADDS.L #1,Reg [0B][0reg]
        val add = data.reg.getLong(op1) + 1
        data.reg.setLong(add, op1)
        ArrayBuffer(data)

      case 0x50 =>
        //INC.W #1,Reg [0B][5reg]
        val reg = data.reg.getWord(op1)
        val inc = reg + 1
        data.reg.setWord(inc, op1)
        check3(reg, new IntSymbol(1), inc, 16, data)

      case 0x70 =>
        //INC.L #1,Reg [0B][7reg]
        val reg = data.reg.getLong(op1)
        val inc = reg + 1
        data.reg.setLong(inc, op1)
        check3(reg, new IntSymbol(1), inc, 32, data)

      case 0x80 =>
        //ADDS.L #2,Reg [0B][8reg]
        val add = data.reg.getLong(op1) + 2
        data.reg.setLong(add, op1)
        ArrayBuffer(data)

      case 0x90 =>
        //ADDS.L #4,Reg [0B][9reg]
        val add = data.reg.getLong(op1) + 4
        data.reg.setLong(add, op1)
        ArrayBuffer(data)

      case 0xD0 =>
        //INC.W #2,Reg [0B][Dreg]
        val reg = data.reg.getWord(op1)
        val inc = reg + 2
        data.reg.setWord(inc, op1)
        check3(reg, new IntSymbol(2), inc, 16, data)

      case 0xF0 =>
        //INC.L #2,Reg [0B][Freg]
        val reg = data.reg.getLong(op1)
        val inc = reg + 1
        data.reg.setLong(inc, op1)
        check3(reg, new IntSymbol(2), inc, 32, data)
    }
  }

  private def analyze0F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    op1 & 0xF0 match {
      //case 0x00 => DAA.B Reg [0F][0reg]

      case _ =>
        //MOV.L RegA,RegB [0F][1regAregB]
        val mov = data.reg.getLong(op1 >> 4)
        data.reg.setLong(mov, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(mov, 32, data)
    }
  }

  private def analyze1(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    rom.getByte(pc) match {
      case 0x10 => analyze10(data, pc)
      case 0x11 => analyze11(data, pc)
      case 0x12 => analyze12(data, pc)
      case 0x13 => analyze13(data, pc)
      case 0x14 =>
        //OR.B RegA,RegB [14][regAregB]
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val and = regB & regA
        data.reg.setByte(and, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(and, 8, data)

      case 0x15 =>
        //XOR.B RegA,RegB [15][regAregB]
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val and = regB & regA
        data.reg.setByte(and, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(and, 8, data)

      case 0x16 =>
        //AND.B RegA,RegB [16][regAregB]
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val and = regB & regA
        data.reg.setByte(and, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(and, 8, data)

      case 0x17 => analyze17(data, pc)
      case 0x18 =>
        //SUB.B RegA,RegB [18][regAregB]
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val sub = regB - regA
        data.reg.setByte(sub, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA.neg, sub, 8, data)

      case 0x19 =>
        //SUB.W RegA,RegB [19][regAregB]
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val sub = regB - regA
        data.reg.setWord(sub, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA.neg, sub, 16, data)

      case 0x1A =>
      case 0x1B =>
      case 0x1C =>
        //CMP.B RegA,RegB [1C][regAregB]
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val sub = regB - regA
        data.pc.setPc(pc + 2)
        check5(regB, regA.neg, sub, 8, data)

      case 0x1D =>
        //CMP.W RegA,RegB [1D][regAregB]
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val sub = regB - regA
        data.pc.setPc(pc + 2)
        check5(regB, regA.neg, sub, 16, data)

      case 0x1E =>
        //SUBX.B RegA,RegB [1E][regAregB]
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val regAx = regA + (if ((ccr & 0x01).eq(0x01)) 1 else 0)
            val sub = regB - regAx
            data.reg.setByte(sub, op1)
            check5(regB, regAx.neg, sub, 8, data)
          case ccr: CtxSymbol =>
            val regAx = regA + ccr.extract(1, 0).zextend(7)
            val sub = regB - regAx
            data.reg.setByte(sub, op1)
            check5(regB, regAx.neg, sub, 8, data)
        }

      case 0x1F =>
    }
  }

  private def analyze10(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.pc.setPc(pc + 2)
    op1 & 0xF0 match {
      case 0x00 =>
        //SHLL.B Reg [10][0reg]
        data.reg.getByte(op1) match {
          case reg: IntSymbol =>
            val shift = reg << 1
            if ((reg & 0x80).eq(0x80)) data.ccr.setC else data.ccr.clearC
            data.reg.setByte(shift, op1)
            check2(shift, 8, data)
          case reg: CtxSymbol =>
            val shift = reg << 1
            data.reg.setByte(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(7, 6)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(7, 6)
            }
            check2(shift, 8, data)
        }

      case 0x01 =>
        //SHLL.W Reg [10][1reg]
        data.reg.getWord(op1) match {
          case reg: IntSymbol =>
            val shift = reg << 1
            if ((reg & 0x8000).eq(0x8000)) data.ccr.setC else data.ccr.clearC
            data.reg.setWord(shift, op1)
            check2(shift, 16, data)
          case reg: CtxSymbol =>
            val shift = reg << 1
            data.reg.setWord(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(15, 14)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(15, 14)
            }
            check2(shift, 16, data)
        }

      case 0x03 =>
        //SHLL.L Reg [10][3reg]
        data.reg.getLong(op1) match {
          case reg: IntSymbol =>
            val shift = reg << 1
            if ((reg & 0x80000000).eq(0x80000000)) data.ccr.setC else data.ccr.clearC
            data.reg.setLong(shift, op1)
            check2(shift, 32, data)
          case reg: CtxSymbol =>
            val shift = reg << 1
            data.reg.setLong(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(31, 30)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(31, 30)
            }
            check2(shift, 32, data)
        }

      case 0x80 =>
        //SHAL.B Reg [10][8reg]
        data.reg.getByte(op1) match {
          case reg: IntSymbol =>
            val shift = reg << 1
            data.reg.setByte(shift, op1)
            if ((reg & 0x80).eq(0x80)) data.ccr.setC else data.ccr.clearC
            check3(reg, reg, shift, 8, data)
          case reg: CtxSymbol =>
            val shift = reg << 1
            data.reg.setByte(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(7, 6)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(7, 6)
            }
            check3(reg, reg, shift, 8, data)
        }

      case 0x90 =>
        //SHAL.W Reg [10][9reg]
        data.reg.getWord(op1) match {
          case reg: IntSymbol =>
            val shift = reg << 1
            data.reg.setWord(shift, op1)
            if ((reg & 0x8000).eq(0x8000)) data.ccr.setC else data.ccr.clearC
            check3(reg, reg, shift, 16, data)
          case reg: CtxSymbol =>
            val shift = reg << 1
            data.reg.setWord(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(15, 14)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(15, 14)
            }
            check3(reg, reg, shift, 16, data)
        }

      case 0xB0 =>
        //SHAL.L Reg [10][Breg]
        data.reg.getLong(op1) match {
          case reg: IntSymbol =>
            val shift = reg << 1
            data.reg.setWord(shift, op1)
            if ((reg & 0x80000000).eq(0x80000000)) data.ccr.setC else data.ccr.clearC
            check3(reg, reg, shift, 32, data)
          case reg: CtxSymbol =>
            val shift = reg << 1
            data.reg.setLong(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(31, 30)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(31, 30)
            }
            check3(reg, reg, shift, 32, data)
        }
    }
  }

  private def analyze11(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.pc.setPc(pc + 2)
    data.ccr.clearV
    op1 & 0xF0 match {
      case 0x00 =>
        //SHLR.B Reg [11][0reg]
        data.ccr.clearN
        data.reg.getByte(op1) match {
          case reg: IntSymbol =>
            val shift = (reg >> 1) & 0x7F
            data.reg.setByte(shift, op1)
            if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
            checkZ(shift, ArrayBuffer(data), 8)
          case reg: CtxSymbol =>
            val shift = new CtxSymbol(ctx.mkBVLshr(reg.symbol, ctx.mkInt(1, reg.symbol.getSort)))
            data.reg.setByte(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(1, 0)
            }
            checkZ(shift, ArrayBuffer(data), 8)
        }

      case 0x01 =>
        //SHLR.W Reg [11][1reg]
        data.ccr.clearN
        data.reg.getWord(op1) match {
          case reg: IntSymbol =>
            val shift = (reg >> 1) & 0x7FFF
            data.reg.setWord(shift, op1)
            if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
            checkZ(shift, ArrayBuffer(data), 16)
          case reg: CtxSymbol =>
            val shift = new CtxSymbol(ctx.mkBVLshr(reg.symbol, ctx.mkInt(1, reg.symbol.getSort)))
            data.reg.setWord(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(1, 0)
            }
            checkZ(shift, ArrayBuffer(data), 16)
        }

      case 0x03 =>
        //SHLR.L Reg [10][3reg]
        data.ccr.clearN
        data.reg.getLong(op1) match {
          case reg: IntSymbol =>
            val shift = (reg >> 1) & 0x7FFFFFFF
            data.reg.setLong(shift, op1)
            if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
            checkZ(shift, ArrayBuffer(data), 32)
          case reg: CtxSymbol =>
            val shift = new CtxSymbol(ctx.mkBVLshr(reg.symbol, ctx.mkInt(1, reg.symbol.getSort)))
            data.reg.setLong(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(1, 0)
            }
            checkZ(shift, ArrayBuffer(data), 32)
        }

      case 0x80 =>
        //SHAR.B Reg [10][8reg]
        data.reg.getByte(op1) match {
          case reg: IntSymbol =>
            val shift = (reg >> 1) | (if ((reg & 0x80).eq(0x80)) 0x80 else 0)
            data.reg.setByte(shift, op1)
            if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
            check2(shift, 8, data)
          case reg: CtxSymbol =>
            val shift = reg >> 1
            data.reg.setByte(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(1, 0)
            }
            check2(shift, 8, ArrayBuffer(data))
        }

      case 0x90 =>
        //SHAR.W Reg [10][9reg]
        data.reg.getWord(op1) match {
          case reg: IntSymbol =>
            val shift = (reg >> 1) | (if ((reg & 0x8000).eq(0x8000)) 0x8000 else 0)
            data.reg.setWord(shift, op1)
            if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
            check2(shift, 16, data)
          case reg: CtxSymbol =>
            val shift = reg >> 1
            data.reg.setWord(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(1, 0)
            }
            check2(shift, 16, ArrayBuffer(data))
        }

      case 0xB0 =>
        //SHAR.L Reg [10][Breg]
        data.reg.getLong(op1) match {
          case reg: IntSymbol =>
            val shift = (reg >> 1) | (if ((reg & 0x80000000).eq(0x80000000)) 0x80000000 else 0)
            data.reg.setLong(shift, op1)
            if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
            check2(shift, 32, data)
          case reg: CtxSymbol =>
            val shift = reg >> 1
            data.reg.setLong(shift, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(1, 0)
            }
            check2(shift, 32, ArrayBuffer(data))
        }
    }
  }

  private def analyze12(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.ccr.clearV
    op1 & 0xF0 match {
      case 0x00 =>
        //ROTXL.B Reg [12][0reg]
        data.reg.getByte(op1) match {
          case reg: IntSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = (reg << 1) | (if ((ccr & 0x01).eq(0x01)) 0x01 else 0)
                data.reg.setByte(rotate, op1)
                if ((reg & 0x80).eq(0x80)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 8, data)
              case ccr: CtxSymbol =>
                val rotate = new CtxSymbol(reg, 8).extract(6, 0) concat ccr.extract(1, 0)
                data.reg.setByte(rotate, op1)
                if ((reg & 0x80).eq(0x80)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 8, data)
            }
          case reg: CtxSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = (reg << 1) | (if ((ccr & 0x01).eq(0x01)) 0x01 else 0)
                data.reg.setByte(rotate, op1)
                data.ccr.ccr = new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(7, 6)
                check2(rotate, 8, data)
              case ccr: CtxSymbol =>
                val rotate = reg.extract(6, 0) concat ccr.extract(1, 0)
                data.reg.setByte(rotate, op1)
                data.ccr.ccr = ccr.extract(7, 1) concat reg.extract(7, 6)
                check2(rotate, 8, data)
            }
        }

      case 0x10 =>
        //ROTXL.W Reg [12][1reg]
        data.reg.getWord(op1) match {
          case reg: IntSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = (reg << 1) | (if ((ccr & 0x01).eq(0x01)) 0x01 else 0)
                data.reg.setWord(rotate, op1)
                if ((reg & 0x8000).eq(0x8000)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 16, data)
              case ccr: CtxSymbol =>
                val rotate = new CtxSymbol(reg, 16).extract(14, 0) concat ccr.extract(1, 0)
                data.reg.setWord(rotate, op1)
                if ((reg & 0x8000).eq(0x8000)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 16, data)
            }
          case reg: CtxSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = (reg << 1) | (if ((ccr & 0x01).eq(0x01)) 0x01 else 0)
                data.reg.setWord(rotate, op1)
                data.ccr.ccr = new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(15, 14)
                check2(rotate, 16, data)
              case ccr: CtxSymbol =>
                val rotate = reg.extract(14, 0) concat ccr.extract(1, 0)
                data.reg.setWord(rotate, op1)
                data.ccr.ccr = ccr.extract(7, 1) concat reg.extract(15, 14)
                check2(rotate, 16, data)
            }
        }

      case 0x30 =>
        //ROTXL.L Reg [12][3reg]
        data.reg.getLong(op1) match {
          case reg: IntSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = (reg << 1) | (if ((ccr & 0x01).eq(0x01)) 0x01 else 0)
                data.reg.setLong(rotate, op1)
                if ((reg & 0x80000000).eq(0x80000000)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 32, data)
              case ccr: CtxSymbol =>
                val rotate = new CtxSymbol(reg, 32).extract(30, 0) concat ccr.extract(1, 0)
                data.reg.setLong(rotate, op1)
                if ((reg & 0x80000000).eq(0x80000000)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 32, data)
            }
          case reg: CtxSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = (reg << 1) | (if ((ccr & 0x01).eq(0x01)) 0x01 else 0)
                data.reg.setLong(rotate, op1)
                data.ccr.ccr = new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(31, 30)
                check2(rotate, 32, data)
              case ccr: CtxSymbol =>
                val rotate = reg.extract(30, 0) concat ccr.extract(1, 0)
                data.reg.setLong(rotate, op1)
                data.ccr.ccr = ccr.extract(7, 1) concat reg.extract(31, 30)
                check2(rotate, 32, data)
            }
        }

      case 0x80 =>
        //ROTL.B Reg [12][8reg]
        data.reg.getByte(op1) match {
          case reg: IntSymbol =>
            val rotate = (reg << 1) | (if ((reg & 0x80).eq(0x80)) 0x01 else 0x00)
            data.reg.setByte(rotate, op1)
            if ((reg & 0x80).eq(0x80)) data.ccr.setC else data.ccr.clearC
            check2(rotate, 8, data)
          case reg: CtxSymbol =>
            val rotate = reg.extract(6, 0) concat reg.extract(7, 6)
            data.reg.setByte(rotate, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(7, 6)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(7, 6)
            }
            check2(rotate, 8, data)
        }

      case 0x90 =>
        //ROTL.W Reg [12][9reg]
        data.reg.getWord(op1) match {
          case reg: IntSymbol =>
            val rotate = (reg << 1) | (if ((reg & 0x8000).eq(0x8000)) 0x01 else 0x00)
            data.reg.setWord(rotate, op1)
            if ((reg & 0x8000).eq(0x8000)) data.ccr.setC else data.ccr.clearC
            check2(rotate, 16, data)
          case reg: CtxSymbol =>
            val rotate = reg.extract(14, 0) concat reg.extract(15, 14)
            data.reg.setWord(rotate, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(15, 14)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(15, 14)
            }
            check2(rotate, 16, data)
        }

      case 0xB0 =>
        //ROTL.L Reg [12][Breg]
        data.reg.getLong(op1) match {
          case reg: IntSymbol =>
            val rotate = (reg << 1) | (if ((reg & 0x80000000).eq(0x80000000)) 0x01 else 0x00)
            data.reg.setLong(rotate, op1)
            if ((reg & 0x80000000).eq(0x80000000)) data.ccr.setC else data.ccr.clearC
            check2(rotate, 32, data)
          case reg: CtxSymbol =>
            val rotate = reg.extract(30, 0) concat reg.extract(31, 30)
            data.reg.setLong(rotate, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(31, 30)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(31, 30)
            }
            check2(rotate, 32, data)
        }
    }
  }

  private def analyze13(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.ccr.clearV
    op1 & 0xF0 match {
      case 0x00 =>
        //ROTXR.B Reg [13][0reg]
        data.reg.getByte(op1) match {
          case reg: IntSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = ((reg >> 1) & 0x7F) | (if ((ccr & 0x01).eq(0x01)) 0x80 else 0)
                data.reg.setByte(rotate, op1)
                if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 8, data)
              case ccr: CtxSymbol =>
                val rotate = ccr.extract(1, 0) concat new CtxSymbol(reg, 8).extract(7, 1)
                data.reg.setByte(rotate, op1)
                if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 8, data)
            }
          case reg: CtxSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = ((reg >> 1) & 0x7F) | (if ((ccr & 0x01).eq(0x01)) 0x80 else 0)
                data.reg.setByte(rotate, op1)
                data.ccr.ccr = new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
                check2(rotate, 8, data)
              case ccr: CtxSymbol =>
                val rotate = ccr.extract(1, 0) concat reg.extract(7, 1)
                data.reg.setByte(rotate, op1)
                data.ccr.ccr = ccr.extract(7, 1) concat reg.extract(1, 0)
                check2(rotate, 8, data)
            }
        }

      case 0x10 =>
        //ROTXR.W Reg [13][1reg]
        data.reg.getWord(op1) match {
          case reg: IntSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = ((reg >> 1) & 0x7FFF) | (if ((ccr & 0x01).eq(0x01)) 0x8000 else 0)
                data.reg.setWord(rotate, op1)
                if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 16, data)
              case ccr: CtxSymbol =>
                val rotate = ccr.extract(1, 0) concat new CtxSymbol(reg, 16).extract(15, 1)
                data.reg.setWord(rotate, op1)
                if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 16, data)
            }
          case reg: CtxSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = ((reg >> 1) & 0x7FFF) | (if ((ccr & 0x01).eq(0x01)) 0x8000 else 0)
                data.reg.setWord(rotate, op1)
                data.ccr.ccr = new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
                check2(rotate, 16, data)
              case ccr: CtxSymbol =>
                val rotate = ccr.extract(1, 0) concat reg.extract(15, 1)
                data.reg.setWord(rotate, op1)
                data.ccr.ccr = ccr.extract(7, 1) concat reg.extract(1, 0)
                check2(rotate, 16, data)
            }
        }

      case 0x30 =>
        //ROTXR.L Reg [13][3reg]
        data.reg.getLong(op1) match {
          case reg: IntSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = ((reg >> 1) & 0x7FFFFFFF) | (if ((ccr & 0x01).eq(0x01)) 0x80000000 else 0)
                data.reg.setLong(rotate, op1)
                if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 32, data)
              case ccr: CtxSymbol =>
                val rotate = ccr.extract(1, 0) concat new CtxSymbol(reg, 32).extract(31, 1)
                data.reg.setLong(rotate, op1)
                if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                check2(rotate, 32, data)
            }
          case reg: CtxSymbol =>
            data.ccr.ccr match {
              case ccr: IntSymbol =>
                val rotate = ((reg >> 1) & 0x7FFFFFFF) | (if ((ccr & 0x01).eq(0x01)) 0x80000000 else 0)
                data.reg.setLong(rotate, op1)
                data.ccr.ccr = new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
                check2(rotate, 32, data)
              case ccr: CtxSymbol =>
                val rotate = ccr.extract(1, 0) concat reg.extract(31, 1)
                data.reg.setLong(rotate, op1)
                data.ccr.ccr = ccr.extract(7, 1) concat reg.extract(1, 0)
                check2(rotate, 32, data)
            }
        }

      case 0x80 =>
        //ROTR.B Reg [13][8reg]
        data.reg.getByte(op1) match {
          case reg: IntSymbol =>
            val rotate = ((reg << 1) & 0x7F) | (if ((reg & 0x01).eq(0x01)) 0x80 else 0)
            data.reg.setByte(rotate, op1)
            if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
            check2(rotate, 8, data)
          case reg: CtxSymbol =>
            val rotate = reg.extract(1, 0) concat reg.extract(7, 1)
            data.reg.setByte(rotate, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(1, 0)
            }
            check2(rotate, 8, data)
        }

      case 0x90 =>
        //ROTR.W Reg [13][9reg]
        data.reg.getWord(op1) match {
          case reg: IntSymbol =>
            val rotate = ((reg << 1) & 0x7FFF) | (if ((reg & 0x01).eq(0x01)) 0x8000 else 0)
            data.reg.setWord(rotate, op1)
            if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
            check2(rotate, 16, data)
          case reg: CtxSymbol =>
            val rotate = reg.extract(1, 0) concat reg.extract(15, 1)
            data.reg.setWord(rotate, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(1, 0)
            }
            check2(rotate, 16, data)
        }

      case 0xB0 =>
        //ROTR.L Reg [13][Breg]
        data.reg.getLong(op1) match {
          case reg: IntSymbol =>
            val rotate = ((reg << 1) & 0x7FFFFFFF) | (if ((reg & 0x01).eq(0x01)) 0x80000000 else 0)
            data.reg.setLong(rotate, op1)
            if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
            check2(rotate, 32, data)
          case reg: CtxSymbol =>
            val rotate = reg.extract(1, 0) concat reg.extract(31, 1)
            data.reg.setLong(rotate, op1)
            data.ccr.ccr = data.ccr.ccr match {
              case ccr: IntSymbol => new CtxSymbol(ccr, 8).extract(7, 1) concat reg.extract(1, 0)
              case ccr: CtxSymbol => ccr.extract(7, 1) concat reg.extract(1, 0)
            }
            check2(rotate, 32, data)
        }
    }
  }

  private def analyze17(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.pc.setPc(pc + 2)
    op1 & 0xF0 match {
      case 0x00 =>
        //NOT.B Reg [17][0reg]
        val not = data.reg.getByte(op1).~
        data.reg.setByte(not, op1)
        data.ccr.clearV
        check2(not, 8, data)

      case 0x10 =>
        //NOT.B Reg [17][1reg]
        val not = data.reg.getWord(op1).~
        data.reg.setWord(not, op1)
        data.ccr.clearV
        check2(not, 8, data)

      case 0x30 =>
        //NOT.B Reg [17][3reg]
        val not = data.reg.getLong(op1).~
        data.reg.setLong(not, op1)
        data.ccr.clearV
        check2(not, 8, data)

      case 0x50 =>
        //EXTU.W Reg [17][5reg]
        val extu = data.reg.getWord(op1) & 0xFF
        data.reg.setWord(extu, op1)
        data.ccr.clearN
        data.ccr.clearV
        checkZ(extu, ArrayBuffer(data), 16)

      case 0x50 =>
        //EXTU.L Reg [17][7reg]
        val extu = data.reg.getLong(op1) & 0xFFFF
        data.reg.setLong(extu, op1)
        data.ccr.clearN
        data.ccr.clearV
        checkZ(extu, ArrayBuffer(data), 32)

      case 0x80 =>
        //NEG.B Reg [17][8reg]
        val reg = data.reg.getByte(op1)
        val neg = reg.neg
        data.reg.setByte(neg, op1)
        check5(new IntSymbol(0), reg.neg, neg, 8, data)

      case 0x90 =>
        //NEG.B Reg [17][9reg]
        val reg = data.reg.getWord(op1)
        val neg = reg.neg
        data.reg.setWord(neg, op1)
        check5(new IntSymbol(0), reg.neg, neg, 16, data)

      case 0xB0 =>
        //NEG.B Reg [17][Breg]
        val reg = data.reg.getLong(op1)
        val neg = reg.neg
        data.reg.setLong(neg, op1)
        check5(new IntSymbol(0), reg.neg, neg, 32, data)

      case 0xD0 =>
        //EXTS.W Reg [17][Dreg]
        val exts = data.reg.getByte(op1) match {
          case reg: IntSymbol => if ((reg & 0x80).eq(0x80)) reg & 0xFF00 else reg & 0x00FF
          case reg: CtxSymbol => reg.sextend(8)
        }
        data.reg.setWord(exts, op1)
        data.ccr.clearV
        check2(exts, 16, data)

      case 0xF0 =>
        //EXTS.L Reg [17][Freg]
        val exts = data.reg.getWord(op1) match {
          case reg: IntSymbol => if ((reg & 0x8000).eq(0x8000)) reg & 0xFFFF0000 else reg & 0x0000FFFF
          case reg: CtxSymbol => reg.sextend(16)
        }
        data.reg.setLong(exts, op1)
        data.ccr.clearV
        check2(exts, 32, data)
    }
  }

  private def analyze1A(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    op1 & 0x80 match {
      case 0x00 =>
        op1 & 0xF0 match {
          case 0x00 =>
            //DEC.B Reg [1A][0reg]
            val reg = data.reg.getByte(op1)
            val dec = reg - 1
            data.reg.setByte(dec, op1)
            data.pc.setPc(pc + 2)
            check3(reg, new IntSymbol(1).neg, dec, 8, data)
        }

      case 0x80 =>
        //SUB.L RegA,RegB [0A][1regA regB]
        val regA = data.reg.getLong(op1 >> 4)
        val regB = data.reg.getLong(op1)
        val sub = regB - regA
        data.reg.setLong(sub, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA.neg, sub, 32, data)
    }
  }

  private def analyze4(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val disp = op1 | (if ((op1 & 0x80) == 0x80) 0xFF00 else 0)
    rom.getByte(pc) match {
      //Bcc [4X][disp]
      case 0x40 =>
        //BRA true
        data.pc.setPc(pc + disp)
        ArrayBuffer(data)

      case 0x42 =>
        //BHI C|V = 0
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            val z = (ccr & 0x04).eq(0x04)
            data.pc.setPc(pc + (if (!(c | z)) disp else 2))
            ArrayBuffer(data)
          case ccr: CtxSymbol =>
            val c = (ccr & 0x01).eq(0x01)
            val z = (ccr & 0x04).eq(0x04)
            val clone = twoPathClone(c || z, data)
            clone._2.pc.setPc(pc + disp)
            clone._1.pc.setPc(pc + 2)
            tapleToArray(clone)
        }

      case 0x45 =>
        //BLO C = 1
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            data.pc.setPc(pc + (if ((ccr & 0x01).eq(0x01)) disp else 2))
            ArrayBuffer(data)
          case ccr: CtxSymbol =>
            val clone = twoPathClone((ccr & 0x01).eq(0x01), data)
            clone._1.pc.setPc(pc + disp)
            clone._2.pc.setPc(pc + 2)
            tapleToArray(clone)
        }

      case 0x4D =>
        //BLT N 排他的論理和 V = 1
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val n = (ccr & 0x08).eq(0x08)
            val v = (ccr & 0x02).eq(0x02)
            data.pc.setPc(pc + (if (n ^ v) disp else 2))
            ArrayBuffer(data)
          case ccr: CtxSymbol =>
            val n = (ccr & 0x08).eq(0x08)
            val v = (ccr & 0x02).eq(0x02)
            val xor = n ^^ v
            val clone = twoPathClone(xor, data)
            clone._1.pc.setPc(pc + disp)
            clone._2.pc.setPc(pc + 2)
            tapleToArray(clone)
        }
    }
  }

  private def analyze5(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc) match {
      case 0x54 =>
        //RTS [54][70]
        val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
        data.reg.setLong(sp + 2, 7)
        data.mem.getWord(sp.symbol) match {
          case reg: IntSymbol =>
            data.pc.setPc(reg.symbol)
            ArrayBuffer(data)
          case reg: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, reg).foreach { p =>
              val clone = pathClone(reg.eq(p), data)
              clone.pc.setPc(p)
              buf += clone
            }
            buf
        }

      case 0x56 =>
        //RTE [56][70]
        val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
        data.reg.setLong(sp + 4, 7)
        data.mem.getLong(sp.symbol) match {
          case reg: IntSymbol =>
            data.pc.setPc(reg.symbol)
            data.ccr.ccr = (reg >> 24) & 0xFF
            ArrayBuffer(data)
          case reg: CtxSymbol =>
            data.ccr.ccr = reg.extract(31, 24)
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, reg.extract(15, 0)).foreach { p =>
              val clone = pathClone(reg.extract(15, 0).eq(p), data)
              clone.pc.setPc(p)
              buf += clone
            }
            buf
        }

      case 0x59 =>
        //JMP IndReg [59][reg0]
        data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
          case abs: IntSymbol =>
            data.pc.setPc(abs.symbol)
            ArrayBuffer(data)
          case abs: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, abs.extract(15, 0)).foreach { p =>
              val clone = pathClone(abs.extract(15, 0).eq(p), data)
              data.pc.setPc(p)
              buf += clone
            }
            buf
        }

      case 0x5E =>
        //JSR Abs:24 [5E][Abs][Abs][Abs]
        val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
        data.mem.setWord(new IntSymbol(pc + 4), (sp - 2).symbol)
        data.reg.setLong(sp - 2, 7)
        data.pc.setPc(rom.getLong(pc))
        ArrayBuffer(data)
    }
  }

  private def analyze6(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    rom.getByte(pc) match {
      case 0x71 =>
        //BNOT.B RegA,RegB [61][regAregB]
        val imm = data.reg.getByte(op1 >> 4)
        data.pc.setPc(pc + 2)
        data.reg.getByte(op1) >> imm match {
          case reg: IntSymbol =>
            val bnot = if ((reg & 0x01).eq(0x01)) reg.bitclr(imm) else reg.bitset(imm)
            data.reg.setByte(bnot, op1)
            ArrayBuffer(data)
          case reg: CtxSymbol =>
            val bool = (reg & 0x01).eq(0x01)
            val clone = twoPathClone(bool, data)
            clone._1.reg.setByte(reg.bitclr(imm), op1)
            clone._2.reg.setByte(reg.bitset(imm), op1)
            tapleToArray(clone)
        }

      case 0x62 =>
        //BCLR.B RegA,RegB [62][regAregB]
        val regA = data.reg.getByte(op1 >> 4) & 0x07
        val regB = data.reg.getByte(op1)
        val bclr = regB.bitclr(regA)
        data.reg.setByte(bclr, op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x66 =>
        //AND.W RegA,RegB [66][regAregB]
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val and = regB & regA
        data.reg.setWord(and, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(and, 16, data)

      case 0x67 =>
        val imm = (op1 >> 4) & 0x07
        val reg = data.reg.getByte(op1)
        data.pc.setPc(pc + 2)
        op1 & 0x80 match {
          case 0x00 =>
            //BST.B Imm,Reg [67][imm reg]
            data.ccr.ccr match {
              case c: IntSymbol =>
                val bst = if ((c & 0x01).eq(0x01)) reg | (1 << imm) else reg & (~(1 << imm))
                data.reg.setByte(bst, op1)
                ArrayBuffer(data)
              case c: CtxSymbol =>
                val clone = twoPathClone((c & 0x01).eq(0x01), data)
                clone._1.reg.setByte(reg | (1 << imm), op1)
                clone._2.reg.setByte(reg & (~(1 << imm)), op1)
                tapleToArray(clone)
            }

          case 0x80 =>
            //BIST.B Imm,Reg [67][1imm reg]
            data.ccr.ccr match {
              case c: IntSymbol =>
                val bst = if ((c & 0x01).eq(0x00)) reg | (1 << imm) else reg & (~(1 << imm))
                data.reg.setByte(bst, op1)
                ArrayBuffer(data)
              case c: CtxSymbol =>
                val clone = twoPathClone((c & 0x01).eq(0x00), data)
                clone._1.reg.setByte(reg | (1 << imm), op1)
                clone._2.reg.setByte(reg & (~(1 << imm)), op1)
                tapleToArray(clone)
            }
        }

      case 0x68 =>
        op1 & 0x80 match {
          case 0x00 =>
            //MOV.B IndReg,Reg [68][0indreg reg]
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            data.reg.getLong(op1 >> 4) match {
              case abs: IntSymbol =>
                val mov = data.mem.getByte(abs.symbol)
                data.reg.setByte(mov, op1)
                check2(mov, 8, data)
              case abs: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, abs.extract(15, 0)).foreach { p =>
                  val clone = pathClone(abs.extract(15, 0).eq(p), data)
                  val mov = clone.mem.getByte(p)
                  clone.reg.setByte(mov, op1)
                  buf ++= check2(mov, 8, data)
                }
                buf
            }

          case 0x80 =>
            //MOV,B Reg,IndReg [68][1indreg reg]
            val mov = data.reg.getByte(op1)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            data.reg.getLong(op1 >> 4) match {
              case abs: IntSymbol =>
                data.mem.setByte(mov, abs.symbol)
                check2(mov, 8, data)
              case abs: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, abs.extract(15, 0).symbol).foreach { p =>
                  val clone = pathClone(abs.extract(15, 0).eq(p), data)
                  clone.mem.setByte(mov, p)
                  buf ++= check2(mov, 8, data)
                }
                buf
            }
        }

      case 0x6A =>
        val abs = rom.getWord(pc + 2)
        data.pc.setPc(pc + 4)
        data.ccr.clearV
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV.B Abs,Reg [6A][0reg][abs][abs]
            val mov = data.mem.getByte(abs)
            data.reg.setByte(mov, op1)
            check2(mov, 8, data)

          case 0x80 =>
            //MOV.B Reg,Abs [6A][8reg][abs][abs]
            val mov = data.reg.getByte(op1)
            data.mem.setByte(mov, abs)
            check2(mov, 8, data)
        }

      case 0x6B =>
        val abs = rom.getWord(pc + 2)
        data.pc.setPc(pc + 4)
        data.ccr.clearV
        op1 & 0xF0 match {
          case 0x00 =>
            //MOV.W Abs,Reg [6B][0Reg][abs][abs]
            val mov = data.mem.getWord(abs)
            data.reg.setWord(mov, op1)
            check2(mov, 16, data)

          case 0x80 =>
            //MOV.W Reg,Abs [6B][8reg][abs][abs]
            val mov = data.reg.getWord(op1)
            data.mem.setWord(mov, abs)
            check2(mov, 16, data)
        }

      case 0x6D =>
        val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        op1 & 0xF0 match {
          case 0x70 =>
            //POP.W Reg [6D][7Reg]
            val pop = data.mem.getWord(sp.symbol)
            data.reg.setWord(pop, op1)
            data.reg.setLong(sp + 2, 7)
            check2(pop, 16, data)

          case 0xF0 =>
            //PUSH.W Reg [6D][FReg]
            val push = data.reg.getWord(op1)
            data.mem.setWord(push, (sp - 2).symbol)
            data.reg.setLong(sp - 2, 7)
            check2(push, 16, data)
        }

      case 0x6E =>
        //MOV.B Disp,Reg [6E][dregreg][disp][disp]
        val disp = rom.getWord(pc + 2)
        data.pc.setPc(pc + 4)
        data.ccr.clearV
        data.reg.getLong(op1 >> 4) + disp match {
          case d: IntSymbol =>
            val mov = data.mem.getByte(d.symbol)
            data.reg.setByte(mov, op1)
            check2(mov, 8, data)
          case d: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
              val clone = pathClone(d.extract(15, 0).eq(p), data)
              val mov = clone.mem.getByte(p)
              clone.reg.setByte(mov, op1)
              buf ++= check2(mov, 8, clone)
            }
            buf
        }
    }
  }

  private def analyze7(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    rom.getByte(pc) match {
      case 0x71 =>
        //BNOT.B Imm,Reg [71][immreg]
        val imm = op1 >> 4
        data.pc.setPc(pc + 2)
        data.reg.getByte(op1) >> imm match {
          case reg: IntSymbol =>
            val bnot = if ((reg & 0x01).eq(0x01)) reg.bitclr(imm) else reg.bitset(imm)
            data.reg.setByte(bnot, op1)
            ArrayBuffer(data)
          case reg: CtxSymbol =>
            val clone = twoPathClone((reg & 0x01).eq(0x01), data)
            clone._1.reg.setByte(reg.bitclr(imm), op1)
            clone._2.reg.setByte(reg.bitset(imm), op1)
            tapleToArray(clone)
        }

      case 0x72 =>
        //BCLR.B Imm,Reg [72][immreg]
        val bclr = data.reg.getByte(op1).bitclr(op1 >> 4)
        data.reg.setByte(bclr, op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x74 =>
        val reg = data.reg.getByte(op1) >> ((op1 >> 4) & 0x07)
        val ccr = data.ccr.ccr
        op1 & 0x80 match {
          case 0x00 =>
            //BOR.B Imm,Reg [74][imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x01)
                if (bool1 | bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (reg & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val clone = twoPathClone((bool1 | bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }

          case 0x80 =>
            //BIOR.B Imm,Reg [74][1imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x00)
                if (bool1 | bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (reg & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                val clone = twoPathClone((bool1 | bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }
        }

      case 0x75 =>
        val reg = data.reg.getByte(op1) >> ((op1 >> 4) & 0x07)
        val ccr = data.ccr.ccr
        data.pc.setPc(pc + 2)
        op1 & 0x80 match {
          case 0x00 =>
            //BXOR.B Imm,Reg [75][0imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x01)
                if (bool1 ^ bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (reg & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val clone = twoPathClone((bool1 ^^ bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }

          case 0x80 =>
            //BIXOR.B Imm,Reg [75][1imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x00)
                if (bool1 ^ bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (reg & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                val clone = twoPathClone((bool1 ^^ bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }
        }

      case 0x76 =>
        val reg = data.reg.getByte(op1) >> ((op1 >> 4) & 0x07)
        val ccr = data.ccr.ccr
        data.pc.setPc(pc + 2)
        op1 & 0x80 match {
          case 0x00 =>
            //BAND.B Imm,Reg [76][0imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x01)
                if (bool1 & bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (reg & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val clone = twoPathClone((bool1 && bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }

          case 0x80 =>
            //BIAND.B Imm,Reg [76][1imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x00)
                if (bool1 & bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (reg & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                val clone = twoPathClone((bool1 && bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }
        }

      case 0x77 =>
        val imm = (op1 >> 4) & 0x07
        data.pc.setPc(pc + 2)
        op1 & 0x80 match {
          case 0x00 =>
            //BLD.B Imm,Reg [77][immreg]
            data.reg.getByte(op1) >> imm match {
              case reg: IntSymbol =>
                if ((reg & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case reg: CtxSymbol =>
                val clone = twoPathClone((reg & 0x01).eq(0x01), data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }

          case 0x80 =>
            //BILD.B Imm,Reg [77][1immreg]
            data.reg.getByte(op1) >> imm match {
              case reg: IntSymbol =>
                if ((reg & 0x01).eq(0x00)) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case reg: CtxSymbol =>
                val clone = twoPathClone((reg & 0x01).eq(0x00), data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }
        }

      case 0x79 => analyze79(data, pc)
      case 0x7A => analyze7A(data, pc)
      case 0x7C => analyze7C(data, pc)
      case 0x7D => analyze7D(data, pc)
      case 0x7E => analyze7E(data, pc)
      case 0x7F => analyze7F(data, pc)

    }
  }

  private def analyze79(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val imm = new IntSymbol(rom.getWord(pc + 2))
    data.pc.setPc(pc + 4)
    op1 & 0xF0 match {
      case 0x00 =>
        //MOV.W Imm,Reg [79][0reg][imm][imm]
        data.reg.setWord(imm, op1)
        data.ccr.clearV
        check2(imm, 16, data)

      case 0x10 =>
        //ADD.W Imm,Reg [79][1reg][imm][imm]
        val reg = data.reg.getWord(op1)
        val add = reg + imm
        data.reg.setWord(add, op1)
        check5(reg, imm, add, 16, data)

      case 0x20 =>
        //CMP.W Imm,Reg [79][2reg][imm][imm]
        val reg = data.reg.getWord(op1)
        val cmp = reg - imm
        check5(reg, imm.neg, cmp, 16, data)

      case 0x60 =>
        //AND.W Imm,Reg [79][6reg][imm][imm]
        val and = data.reg.getWord(op1) & imm
        data.reg.setWord(and, op1)
        data.ccr.clearV
        check2(and, 16, data)
    }
  }

  private def analyze7A(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val imm = new IntSymbol(rom.getLong(pc + 2))
    data.pc.setPc(pc + 6)
    op1 & 0xF0 match {
      case 0x00 =>
        //MOV,L Imm,Reg [7A][0reg][imm][imm][imm][imm]
        data.reg.setLong(imm, op1)
        data.ccr.clearV
        check2(imm, 32, data)

      case 0x60 =>
        //AND.L Imm,Reg [7A][6reg][imm][imm][imm][imm]
        val and = data.reg.getLong(op1) & imm
        data.reg.setLong(and, op1)
        data.ccr.clearV
        check2(imm, 32, data)
    }
  }

  private def analyze7C(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val ccr = data.ccr.ccr
    data.pc.setPc(pc + 4)
    rom.getByte(pc + 2) match {
      case 0x74 =>
        op3 & 0x80 match {
          case 0x00 =>
            //BOR.B Imm,IndReg [7C][indreg0][74][imm0]
            data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
              case reg: IntSymbol =>
                val mem = data.mem.getByte(reg.symbol) >> imm
                (ccr, mem) match {
                  case (c: IntSymbol, m: IntSymbol) =>
                    val bool1 = (c & 0x01).eq(0x01)
                    val bool2 = (m & 0x01).eq(0x01)
                    if (bool1 | bool2) data.ccr.setC else data.ccr.clearC
                    ArrayBuffer(data)
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                    val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                    val clone = twoPathClone((bool1 || bool2).asInstanceOf[CtxSymbol], data)
                    clone._1.ccr.setC
                    clone._2.ccr.clearC
                    tapleToArray(clone)
                }
              case reg: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach { p =>
                  val clone = pathClone(reg.extract(15, 0).eq(p), data)
                  val mem = clone.mem.getByte(p) >> imm
                  (ccr, mem) match {
                    case (c: IntSymbol, m: IntSymbol) =>
                      val bool1 = (c & 0x01).eq(0x01)
                      val bool2 = (m & 0x01).eq(0x01)
                      if (bool1 | bool2) clone.ccr.setC else clone.ccr.clearC
                      buf += clone
                    case _ =>
                      val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                      val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                      val clone = twoPathClone((bool1 || bool2).asInstanceOf[CtxSymbol], data)
                      clone._1.ccr.setC
                      clone._2.ccr.clearC
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }

          case 0x80 =>
            //BIOR.B Imm,IndReg [7C][indreg0][74][1imm0]
            data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
              case reg: IntSymbol =>
                val mem = data.mem.getByte(reg.symbol) >> imm
                (ccr, mem) match {
                  case (c: IntSymbol, m: IntSymbol) =>
                    val bool1 = (c & 0x01).eq(0x01)
                    val bool2 = (m & 0x01).eq(0x00)
                    if (bool1 | bool2) data.ccr.setC else data.ccr.clearC
                    ArrayBuffer(data)
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                    val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                    val clone = twoPathClone((bool1 || bool2).asInstanceOf[CtxSymbol], data)
                    clone._1.ccr.setC
                    clone._2.ccr.clearC
                    tapleToArray(clone)
                }
              case reg: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach { p =>
                  val clone = pathClone(reg.extract(15, 0).eq(p), data)
                  val mem = clone.mem.getByte(p) >> imm
                  (ccr, mem) match {
                    case (c: IntSymbol, m: IntSymbol) =>
                      val bool1 = (c & 0x01).eq(0x01)
                      val bool2 = (m & 0x01).eq(0x00)
                      if (bool1 | bool2) clone.ccr.setC else clone.ccr.clearC
                      buf += clone
                    case _ =>
                      val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                      val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                      val clone = twoPathClone((bool1 || bool2).asInstanceOf[CtxSymbol], data)
                      clone._1.ccr.setC
                      clone._2.ccr.clearC
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }
        }

      case 0x75 =>
        op3 & 0x80 match {
          case 0x00 =>
            //BXOR.B Imm,IndReg [7C][indreg0][75][imm0]
            data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
              case reg: IntSymbol =>
                val mem = data.mem.getByte(reg.symbol) >> imm
                (ccr, mem) match {
                  case (c: IntSymbol, m: IntSymbol) =>
                    val bool1 = (c & 0x01).eq(0x01)
                    val bool2 = (m & 0x01).eq(0x01)
                    if (bool1 ^ bool2) data.ccr.setC else data.ccr.clearC
                    ArrayBuffer(data)
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                    val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                    val clone = twoPathClone((bool1 ^^ bool2).asInstanceOf[CtxSymbol], data)
                    clone._1.ccr.setC
                    clone._2.ccr.clearC
                    tapleToArray(clone)
                }
              case reg: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach { p =>
                  val clone = pathClone(reg.extract(15, 0).eq(p), data)
                  val mem = clone.mem.getByte(p) >> imm
                  (ccr, mem) match {
                    case (c: IntSymbol, m: IntSymbol) =>
                      val bool1 = (c & 0x01).eq(0x01)
                      val bool2 = (m & 0x01).eq(0x01)
                      if (bool1 ^ bool2) clone.ccr.setC else clone.ccr.clearC
                      buf += clone
                    case _ =>
                      val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                      val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                      val clone = twoPathClone((bool1 ^^ bool2).asInstanceOf[CtxSymbol], data)
                      clone._1.ccr.setC
                      clone._2.ccr.clearC
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }

          case 0x80 =>
            //BIXOR.B Imm,IndReg [7C][indreg0][75][1imm0]
            data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
              case reg: IntSymbol =>
                val mem = data.mem.getByte(reg.symbol) >> imm
                (ccr, mem) match {
                  case (c: IntSymbol, m: IntSymbol) =>
                    val bool1 = (c & 0x01).eq(0x01)
                    val bool2 = (m & 0x01).eq(0x00)
                    if (bool1 ^ bool2) data.ccr.setC else data.ccr.clearC
                    ArrayBuffer(data)
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                    val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                    val clone = twoPathClone((bool1 ^^ bool2).asInstanceOf[CtxSymbol], data)
                    clone._1.ccr.setC
                    clone._2.ccr.clearC
                    tapleToArray(clone)
                }
              case reg: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach { p =>
                  val clone = pathClone(reg.extract(15, 0).eq(p), data)
                  val mem = clone.mem.getByte(p) >> imm
                  (ccr, mem) match {
                    case (c: IntSymbol, m: IntSymbol) =>
                      val bool1 = (c & 0x01).eq(0x01)
                      val bool2 = (m & 0x01).eq(0x00)
                      if (bool1 ^ bool2) clone.ccr.setC else clone.ccr.clearC
                      buf += clone
                    case _ =>
                      val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                      val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                      val clone = twoPathClone((bool1 ^^ bool2).asInstanceOf[CtxSymbol], data)
                      clone._1.ccr.setC
                      clone._2.ccr.clearC
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }
        }

      case 0x76 =>
        op3 & 0x80 match {
          case 0x00 =>
            //BAND.B Imm,IndReg [7C][indreg0][76][imm0]
            data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
              case reg: IntSymbol =>
                val mem = data.mem.getByte(reg.symbol) >> imm
                (ccr, mem) match {
                  case (c: IntSymbol, m: IntSymbol) =>
                    val bool1 = (c & 0x01).eq(0x01)
                    val bool2 = (m & 0x01).eq(0x01)
                    if (bool1 & bool2) data.ccr.setC else data.ccr.clearC
                    ArrayBuffer(data)
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                    val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                    val clone = twoPathClone((bool1 && bool2).asInstanceOf[CtxSymbol], data)
                    clone._1.ccr.setC
                    clone._2.ccr.clearC
                    tapleToArray(clone)
                }
              case reg: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach { p =>
                  val clone = pathClone(reg.extract(15, 0).eq(p), data)
                  val mem = clone.mem.getByte(p) >> imm
                  (ccr, mem) match {
                    case (c: IntSymbol, m: IntSymbol) =>
                      val bool1 = (c & 0x01).eq(0x01)
                      val bool2 = (m & 0x01).eq(0x01)
                      if (bool1 & bool2) clone.ccr.setC else clone.ccr.clearC
                      buf += clone
                    case _ =>
                      val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                      val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                      val clone = twoPathClone((bool1 && bool2).asInstanceOf[CtxSymbol], data)
                      clone._1.ccr.setC
                      clone._2.ccr.clearC
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }

          case 0x80 =>
            //BIAND.B Imm,IndReg [7C][indreg0][76][1imm0]
            data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
              case reg: IntSymbol =>
                val mem = data.mem.getByte(reg.symbol) >> imm
                (ccr, mem) match {
                  case (c: IntSymbol, m: IntSymbol) =>
                    val bool1 = (c & 0x01).eq(0x01)
                    val bool2 = (m & 0x01).eq(0x00)
                    if (bool1 & bool2) data.ccr.setC else data.ccr.clearC
                    ArrayBuffer(data)
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                    val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                    val clone = twoPathClone((bool1 && bool2).asInstanceOf[CtxSymbol], data)
                    clone._1.ccr.setC
                    clone._2.ccr.clearC
                    tapleToArray(clone)
                }
              case reg: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach { p =>
                  val clone = pathClone(reg.extract(15, 0).eq(p), data)
                  val mem = clone.mem.getByte(p) >> imm
                  (ccr, mem) match {
                    case (c: IntSymbol, m: IntSymbol) =>
                      val bool1 = (c & 0x01).eq(0x01)
                      val bool2 = (m & 0x01).eq(0x00)
                      if (bool1 & bool2) clone.ccr.setC else clone.ccr.clearC
                      buf += clone
                    case _ =>
                      val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                      val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                      val clone = twoPathClone((bool1 && bool2).asInstanceOf[CtxSymbol], data)
                      clone._1.ccr.setC
                      clone._2.ccr.clearC
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }
        }

      case 0x77 =>
        op3 & 0x80 match {
          case 0x00 =>
            //BLD.B Imm,IndReg [7C][indreg0][77][imm0]
            data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
              case ind: IntSymbol =>
                data.mem.getByte(ind.symbol) >> imm match {
                  case m: IntSymbol =>
                    if ((m & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                    ArrayBuffer(data)
                  case m: CtxSymbol =>
                    val bool = (m & 0x01).eq(0x01)
                    val clone = twoPathClone(bool, data)
                    clone._1.ccr.setC
                    clone._2.ccr.clearC
                    tapleToArray(clone)
                }
              case ind: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = pathClone(ind.extract(15, 0).eq(p), data)
                  clone.mem.getByte(p) >> imm match {
                    case m: IntSymbol =>
                      if ((m & 0x01).eq(0x01)) clone.ccr.setC else clone.ccr.clearC
                      buf += clone
                    case m: CtxSymbol =>
                      val clone = twoPathClone((m & 0x01).eq(0x01), data)
                      clone._1.ccr.setC
                      clone._2.ccr.clearC
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }

          case 0x80 =>
            //BILD.B Imm,IndReg [7C][indreg0][77][1imm0]
            data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
              case ind: IntSymbol =>
                data.mem.getByte(ind.symbol) >> imm match {
                  case m: IntSymbol =>
                    if ((m & 0x01).eq(0x00)) data.ccr.setC else data.ccr.clearC
                    ArrayBuffer(data)
                  case m: CtxSymbol =>
                    val clone = twoPathClone((m & 0x01).eq(0x00), data)
                    clone._1.ccr.setC
                    clone._2.ccr.clearC
                    tapleToArray(clone)
                }
              case ind: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = pathClone(ind.extract(15, 0).eq(p), data)
                  clone.mem.getByte(p) >> imm match {
                    case m: IntSymbol =>
                      if ((m & 0x01).eq(0x00)) clone.ccr.setC else clone.ccr.clearC
                      buf += clone
                    case m: CtxSymbol =>
                      val clone = twoPathClone((m & 0x01).eq(0x00), data)
                      clone._1.ccr.setC
                      clone._2.ccr.clearC
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }
        }
    }
  }

  private def analyze7D(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    data.pc.setPc(pc + 4)
    val op1 = rom.getByte(pc + 1)
    val op3 = rom.getByte(pc + 3)
    rom.getByte(pc + 2) match {
      case 0x62 =>
        //BCLR.B Reg,IndReg [7D][indreg0][62][reg0]
        val reg = data.reg.getByte(op3 >> 4) & 0x07
        data.reg.getByte(op1 >> 4) match {
          case ind: IntSymbol =>
            val mem = data.mem.getByte(ind.symbol)
            val bclr = mem.bitclr(reg)
            data.mem.setByte(bclr, ind.symbol)
            ArrayBuffer(data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              val mem = clone.mem.getByte(p)
              val bclr = mem.bitclr(reg)
              clone.mem.setByte(bclr, p)
              buf += clone
            }
            buf
        }

      case 0x67 =>
        val imm = (op3 >> 4) & 0x07
        op3 & 0x80 match {
          case 0x00 =>
            //BST.B Imm,Indreg [7D][reg0][67][imm0]
            data.reg.getByte(op1 >> 4) match {
              case ind: IntSymbol =>
                val mem = data.mem.getByte(ind.symbol)
                data.ccr.ccr match {
                  case ccr: IntSymbol =>
                    val bst = if ((ccr & 0x01).eq(0x01)) mem | (1 << imm) else mem & (~(1 << imm))
                    data.mem.setByte(bst, ind.symbol)
                    ArrayBuffer(data)
                  case ccr: CtxSymbol =>
                    val clone = twoPathClone((ccr & 0x01).eq(0x01), data)
                    clone._1.mem.setByte(mem | (1 << imm), ind.symbol)
                    clone._2.mem.setByte(mem & (~(1 << imm)), ind.symbol)
                    tapleToArray(clone)
                }
              case ind: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = pathClone(ind.extract(15, 0).eq(p), data)
                  val mem = clone.mem.getByte(p)
                  data.ccr.ccr match {
                    case ccr: IntSymbol =>
                      val bst = if ((ccr & 0x01).eq(0x01)) mem | (1 << imm) else mem & (~(1 << imm))
                      clone.mem.setByte(bst, p)
                      buf += clone
                    case ccr: CtxSymbol =>
                      val clone = twoPathClone((ccr & 0x01).eq(0x01), data)
                      clone._1.mem.setByte(mem | (1 << imm), p)
                      clone._2.mem.setByte(mem & (~(1 << imm)), p)
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }

          case 0x80 =>
            //BIST.B Imm,Indreg [7D][reg0][67][1imm0]]
            data.reg.getByte(op1 >> 4) match {
              case ind: IntSymbol =>
                val mem = data.mem.getByte(ind.symbol)
                data.ccr.ccr match {
                  case ccr: IntSymbol =>
                    val bst = if ((ccr & 0x01).eq(0x00)) mem | (1 << imm) else mem & (~(1 << imm))
                    data.mem.setByte(bst, ind.symbol)
                    ArrayBuffer(data)
                  case ccr: CtxSymbol =>
                    val clone = twoPathClone((ccr & 0x01).eq(0x00), data)
                    clone._1.mem.setByte(mem | (1 << imm), ind.symbol)
                    clone._2.mem.setByte(mem & (~(1 << imm)), ind.symbol)
                    tapleToArray(clone)
                }
              case ind: CtxSymbol =>
                val buf = new ArrayBuffer[DataSet]
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = pathClone(ind.extract(15, 0).eq(p), data)
                  val mem = clone.mem.getByte(p)
                  data.ccr.ccr match {
                    case ccr: IntSymbol =>
                      val bst = if ((ccr & 0x01).eq(0x00)) mem | (1 << imm) else mem & (~(1 << imm))
                      clone.mem.setByte(bst, p)
                      buf += clone
                    case ccr: CtxSymbol =>
                      val clone = twoPathClone((ccr & 0x01).eq(0x00), data)
                      clone._1.mem.setByte(mem | (1 << imm), p)
                      clone._2.mem.setByte(mem & (~(1 << imm)), p)
                      buf ++= tapleToArray(clone)
                  }
                }
                buf
            }
        }

      case 0x71 =>
        //BNOT.B Imm,IndReg [7D][immreg][0x71][imm0]
        val imm = op3 >> 4
        data.reg.getByte(op1 >> 4) match {
          case ind: IntSymbol =>
            data.mem.getByte(ind.symbol) >> imm match {
              case mem: IntSymbol =>
                val bnot = if ((mem & 0x01).eq(0x01)) mem.bitclr(imm) else mem.bitset(imm)
                data.reg.setByte(bnot, ind.symbol)
                ArrayBuffer(data)
              case mem: CtxSymbol =>
                val bool = (mem & 0x01).eq(0x01)
                val clone = twoPathClone(bool, data)
                clone._1.reg.setByte(mem.bitclr(imm), ind.symbol)
                clone._2.reg.setByte(mem.bitset(imm), ind.symbol)
                tapleToArray(clone)
            }
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              clone.mem.getByte(p) >> imm match {
                case mem: IntSymbol =>
                  val bnot = if ((mem & 0x01).eq(0x01)) mem.bitclr(imm) else mem.bitset(imm)
                  clone.reg.setByte(bnot, p)
                  buf += clone
                case mem: CtxSymbol =>
                  val bool = (mem & 0x01).eq(0x01)
                  val clone = twoPathClone(bool, data)
                  clone._1.reg.setByte(mem.bitclr(imm), p)
                  clone._2.reg.setByte(mem.bitset(imm), p)
                  buf ++= tapleToArray(clone)
              }
            }
            buf
        }

      case 0x72 =>
        //BCLR.B Imm,Indreg [7D][reg0][72][imm0]
        val imm = op3 >> 4
        data.reg.getLong(op1 >> 4) match {
          case ind: IntSymbol =>
            val mem = data.mem.getByte(ind.symbol)
            val bclr = mem.bitclr(imm)
            data.mem.setByte(bclr, ind.symbol)
            ArrayBuffer(data)
          case ind: CtxSymbol =>
            val buf = new ArrayBuffer[DataSet]
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = pathClone(ind.extract(15, 0).eq(p), data)
              val mem = clone.mem.getByte(p)
              val bclr = mem.bitclr(imm)
              clone.mem.setByte(bclr, p)
              buf += clone
            }
            buf
        }
    }
  }

  private def analyze7E(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    rom.getByte(pc + 2) match {
      case 0x74 =>
        val mem = data.mem.getByte(rom.getByte(pc + 1)) >> imm
        val ccr = data.ccr.ccr
        op3 & 0x80 match {
          case 0x00 =>
            //BOR.B Imm,Abs [7E][abs][74][imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (m & 0x01).eq(0x01)
                if (bool1 | bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val clone = twoPathClone((bool1 || bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }

          case 0x80 =>
            //BIOR.B Imm,Abs [7E][abs][74][1imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (m & 0x01).eq(0x00)
                if (bool1 | bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                val clone = twoPathClone((bool1 || bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }
        }

      case 0x75 =>
        val mem = data.mem.getByte(rom.getByte(pc + 1)) >> imm
        val ccr = data.ccr.ccr
        op3 & 0x80 match {
          case 0x00 =>
            //BXOR.B Imm,Abs [7E][abs][75][imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (m & 0x01).eq(0x01)
                if (bool1 ^ bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val clone = twoPathClone((bool1 ^^ bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }

          case 0x80 =>
            //BIXOR.B Imm,Abs [7E][abs][75][1imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (m & 0x01).eq(0x00)
                if (bool1 ^ bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                val clone = twoPathClone((bool1 ^^ bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }
        }

      case 0x76 =>
        val mem = data.mem.getByte(rom.getByte(pc + 1)) >> imm
        val ccr = data.ccr.ccr
        op3 & 0x80 match {
          case 0x00 =>
            //BAND.B Imm,Abs [7E][abs][76][imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (m & 0x01).eq(0x01)
                if (bool1 & bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val clone = twoPathClone((bool1 && bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }

          case 0x80 =>
            //BIAND.B Imm,Abs [7E][abs][76][1imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (m & 0x01).eq(0x00)
                if (bool1 & bool2) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case _ =>
                val bool1 = (ccr & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x01, 8))
                val bool2 = (mem & new CtxSymbol(0x01, 8)).eq(new CtxSymbol(0x00, 8))
                val clone = twoPathClone((bool1 && bool2).asInstanceOf[CtxSymbol], data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }
        }

      case 0x77 =>
        op3 & 0x80 match {
          case 0x00 =>
            //BLD.B Imm,Abs [7E][abs][77][imm0]
            data.mem.getByte(rom.getByte(pc + 1)) >> imm match {
              case mem: IntSymbol =>
                if ((mem & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case mem: CtxSymbol =>
                val clone = twoPathClone((mem & 0x01).eq(0x01), data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }

          case 0x80 =>
            //BILD.B Imm,Abs [7E][abs][77][1imm0]
            data.mem.getByte(rom.getByte(pc + 1)) >> imm match {
              case mem: IntSymbol =>
                if ((mem & 0x01).eq(0x00)) data.ccr.setC else data.ccr.clearC
                ArrayBuffer(data)
              case mem: CtxSymbol =>
                val clone = twoPathClone((mem & 0x01).eq(0x00), data)
                clone._1.ccr.setC
                clone._2.ccr.clearC
                tapleToArray(clone)
            }
        }
    }
  }

  private def analyze7F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val abs = rom.getByte(pc + 1) | 0xFFFF00
    val op3 = rom.getByte(pc + 3)
    data.pc.setPc(pc + 4)
    rom.getByte(pc + 2) match {
      case 0x62 =>
        //BCLR.B Reg,Abs [7F][abs][62][reg0]
        val reg = data.reg.getByte(op3 >> 4) & 0x07
        val bclr = data.mem.getByte(abs).bitclr(reg)
        data.mem.setByte(bclr, abs)
        ArrayBuffer(data)

      case 0x67 =>
        val imm = (op3 >> 4) & 0x07
        val mem = data.mem.getByte(abs)
        op3 & 0x80 match {
          case 0x00 =>
            //BST.B Imm,Abs [7F][abs][67][Imm0]
            data.ccr.ccr match {
              case c: IntSymbol =>
                val bst = if ((c & 0x01).eq(0x01)) mem | (1 << imm) else mem & (~(1 << imm))
                data.mem.setByte(bst, abs)
                ArrayBuffer(data)
              case c: CtxSymbol =>
                val clone = twoPathClone((c & 0x01).eq(0x01), data)
                clone._1.mem.setByte(mem | (1 << imm), abs)
                clone._2.mem.setByte(mem & (~(1 << imm)), abs)
                tapleToArray(clone)
            }

          case 0x80 =>
            //BIST.B Imm,Abs [7F][abs][67][1Imm0]
            data.ccr.ccr match {
              case c: IntSymbol =>
                val bst = if ((c & 0x01).eq(0x00)) mem | (1 << imm) else mem & (~(1 << imm))
                data.mem.setByte(bst, abs)
                ArrayBuffer(data)
              case c: CtxSymbol =>
                val clone = twoPathClone((c & 0x01).eq(0x00), data)
                clone._1.mem.setByte(mem | (1 << imm), abs)
                clone._2.mem.setByte(mem & (~(1 << imm)), abs)
                tapleToArray(clone)
            }
        }

      case 0x70 =>
        //BSET.B Imm,Abs [7F][abs][70][Imm0]
        val imm = rom.getByte(pc + 3) >> 4
        val bset = data.mem.getByte(abs).bitset(imm)
        data.mem.setByte(bset, abs)
        ArrayBuffer(data)

      case 0x72 =>
        //BSET.B Imm,Abs [7F][abs][72][Imm0]
        val imm = rom.getByte(pc + 3) >> 4
        val bclr = data.mem.getByte(abs).bitclr(imm)
        data.mem.setByte(bclr, abs)
        ArrayBuffer(data)
    }
  }

  private def extract(range: Range, ast: Z3AST): ArrayBuffer[Int] = {
    val buf = new ArrayBuffer[Int]
    range.foreach { n =>
      val s = ctx.mkSolver
      s.assertCnstr(ctx.mkEq(ast, ctx.mkInt(n, ast.getSort)))
      if (s.check.get) buf += n
    }
    buf
  }

  def extract(range: Range, c: CtxSymbol): ArrayBuffer[Int] = extract(range, c.symbol)

  def check2(data: MySymbol, size: Int, base: DataSet): ArrayBuffer[DataSet] = check2(data, size, ArrayBuffer(base))

  def check2(data: MySymbol, size: Int, base: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val buf = checkZ(data, base, size)
    checkN(data, buf, size)
  }

  def check3(data1: MySymbol, data2: MySymbol, result: MySymbol, size: Int, base: DataSet): ArrayBuffer[DataSet] = check3(data1, data2, result, size, ArrayBuffer(base))

  def check3(data1: MySymbol, data2: MySymbol, result: MySymbol, size: Int, base: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    var buf = checkV(data1, data2, result, base, size)
    check2(result, size, buf)
  }

  def check5(data1: MySymbol, data2: MySymbol, result: MySymbol, size: Int, base: DataSet): ArrayBuffer[DataSet] = {
    var buf = checkC(data1, data2, result, ArrayBuffer(base), size)
    buf = check3(data1, data2, result, size, buf)
    checkH(data1, data2, result, buf, size)
  }

  private def checkC(data1: MySymbol, data2: MySymbol, res: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    val and = size match {
      case 8 => 0x80
      case 16 => 0x8000
      case 32 => 0x80000000
    }
    buf.foreach { b =>
      (data1, data2, res) match {
        case (d1: IntSymbol, d2: IntSymbol, r: IntSymbol) =>
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and)
          val bool2 = ((d1 & and).eq(and) || (d2 & and).eq(and)) && ((r & and).eq(0))
          if (bool1 | bool2) b.ccr.setC else b.ccr.clearC
          ans += b
        case _ =>
          val bool1 = (data1 & and).eq(new CtxSymbol(and, size)) && (data2 & and).eq(new CtxSymbol(and, size))
          val bool2 = ((data1 & and).eq(new CtxSymbol(and, size)) || (data2 & and).eq(new CtxSymbol(and, size))) && ((res & and).eq(new CtxSymbol(0, size)))
          val clone = twoPathClone((bool1 || bool2).asInstanceOf[CtxSymbol], b)
          clone._1.ccr.setC
          clone._2.ccr.clearC
          ans ++= tapleToArray(clone)
      }
    }
    ans
  }


  private def checkV(data1: MySymbol, data2: MySymbol, res: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b =>
      (data1, data2, res) match {
        case (d1: IntSymbol, d2: IntSymbol, r: IntSymbol) =>
          val and = size match {
            case 8 => 0x80
            case 16 => 0x8000
            case 32 => 0x80000000
          }
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and) && (r & and).eq(0)
          val bool2 = (d1 & and).eq(0) && (d2 & and).eq(0) && (r & and).eq(and)
          if (bool1 | bool2) b.ccr.setV else b.ccr.clearV
          ans += b
        case _ =>
          val bool1 = data1 >= new CtxSymbol(0, size) && data2 >= new CtxSymbol(0, size) && res < new CtxSymbol(0, size)
          val bool2 = data1 < new CtxSymbol(0, size) && data2 < new CtxSymbol(0, size) && res >= new CtxSymbol(0, size)
          val clone = twoPathClone((bool1 || bool2).asInstanceOf[CtxSymbol], b)
          clone._1.ccr.setV
          clone._2.ccr.clearV
          ans ++= tapleToArray(clone)
      }
    }
    ans
  }

  private def checkZ(data: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b =>
      data match {
        case d: IntSymbol =>
          val and = size match {
            case 8 => 0xFF
            case 16 => 0xFFFF
            case 32 => 0xFFFFFFFF
          }
          if ((d & and).eq(0)) b.ccr.setZ else b.ccr.clearZ
          ans += b
        case d: CtxSymbol =>
          val clone = twoPathClone(d.eq(0), b)
          clone._1.ccr.setZ //data = 0
          clone._2.ccr.clearZ
          ans ++= tapleToArray(clone)
      }
    }
    ans
  }

  private def checkN(data: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b =>
      data match {
        case d: IntSymbol =>
          val and = size match {
            case 8 => 0x80
            case 16 => 0x8000
            case 32 => 0x80000000
          }
          if ((d & and).eq(and)) b.ccr.setN else b.ccr.clearN
          ans += b
        case d: CtxSymbol =>
          val clone = twoPathClone(d < 0, b)
          clone._1.ccr.setN //data < 0
          clone._2.ccr.clearN
          ans ++= tapleToArray(clone)
      }
    }
    ans
  }

  private def checkH(data1: MySymbol, data2: MySymbol, res: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    val and = size match {
      case 8 => 0x08
      case 16 => 0x0800
      case 32 => 0x00800000
    }
    buf.foreach { b =>
      (data1, data2, res) match {
        case (d1: IntSymbol, d2: IntSymbol, r: IntSymbol) =>
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and)
          val bool2 = ((d1 & and).eq(and) || (d2 & and).eq(and)) && ((r & and).eq(0))
          if (bool1 | bool2) b.ccr.setH else b.ccr.clearH
          ans += b
        case _ =>
          val bool1 = (data1 & and).eq(new CtxSymbol(and, size)) && (data2 & and).eq(new CtxSymbol(and, size))
          val bool2 = ((data1 & and).eq(new CtxSymbol(and, size)) || (data2 & and).eq(new CtxSymbol(and, size))) &&
            ((res & and).eq(new CtxSymbol(0, size)))
          val clone = twoPathClone((bool1 || bool2).asInstanceOf[CtxSymbol], b)
          clone._1.ccr.setH
          clone._2.ccr.clearH
          ans ++= tapleToArray(clone)
      }
    }
    ans
  }

  private def tapleToArray(data: (DataSet, DataSet)): ArrayBuffer[DataSet] = ArrayBuffer(data._1, data._2)

  private def pathClone(path: CtxSymbol, data: DataSet): DataSet = {
    val clone = data.clone
    clone.path.set(path.symbol)
    clone
  }

  private def twoPathClone(path: CtxSymbol, data: DataSet): (DataSet, DataSet) = {
    val clone1 = data.clone
    clone1.path.set(path.symbol)
    val clone2 = data.clone
    clone2.path.set(path.not.symbol)
    clone1 -> clone2
  }

}
