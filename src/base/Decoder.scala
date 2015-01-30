package base

import data.DataSet
import symbol.CtxSymbol
import z3.scala.Z3AST

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/17.
 */
class Decoder {

  private val ctx = Symbolic.ctx
  private val rom = Symbolic.rom

  //記号実行してる
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
    val imm = new CtxSymbol(rom.getByte(pc + 1), 8)
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
        val immx = imm + data.ccr.getCcr.extract(0, 0).zextend(7)
        val add = reg + immx
        data.reg.setByte(add, op0)
        check5(reg, immx, add, 8, data)

      case 0xA0 =>
        //CMP.B Imm,Reg [Areg][imm]
        val reg = data.reg.getByte(op0)
        val cmp = reg - imm
        check5(reg, imm.neg, cmp, 8, data)

      case 0xB0 =>
        //SUBX.B Imm,Reg [Breg][imm]
        val reg = data.reg.getByte(op0)
        val immx = imm + data.ccr.getCcr.extract(0, 0).zextend(7)
        val add = reg - immx
        data.reg.setByte(add, op0)
        check5(reg, immx.neg, add, 8, data)

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
    val op1 = rom.getByte(pc + 1)
    rom.getByte(pc) match {
      case 0x00 =>
        //NOP [00][00]
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x01 => analyze01(data, pc)

      case 0x02 =>
        //STC.B CCR,Reg [02][0reg]
        data.reg.setByte(data.ccr.getCcr, op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x03 =>
        //LDC.B Reg,CCR [03][0reg]
        data.ccr.setCcr(data.reg.getByte(op1))
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x04 =>
        //ORC.B Imm,Ccr [04][imm]
        data.ccr.setCcr(data.ccr.getCcr | op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x05 =>
        //ANDC.B Imm,Ccr [06][imm]
        data.ccr.setCcr(data.ccr.getCcr ^ op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x06 =>
        //ANDC.B Imm,Ccr [06][imm]
        data.ccr.setCcr(data.ccr.getCcr & op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x07 =>
        //LDC.B Reg,CCR [03][imm]
        data.ccr.setCcr(new CtxSymbol(op1, 8))
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
        val carry = data.ccr.getCcr.extract(0, 0).zextend(7)
        val imm = regA + carry
        val add = regB + imm
        data.reg.setByte(add, op1)
        check5(regB, imm, add, 8, data)

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
    val ind = data.reg.getLong(op3 >> 4)
    data.pc.setPc(pc + 4)
    data.ccr.clearV
    op3 & 0x80 match {
      case 0x00 =>
        //MOV.L Indreg,Reg [01][00][69][indreg reg]
        val mov = data.mem.getLong(ind)
        data.reg.setLong(mov, op3)
        check2(mov, 32, data)

      case 0x80 =>
        //MOV.L Reg,IndReg [01][00][69][1indreg reg]
        val mov = data.reg.getLong(op3)
        data.mem.setLong(mov, ind)
        check2(mov, 32, data)
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
    val ind = data.reg.getLong(op3 >> 4)
    data.pc.setPc(pc + 4)
    data.ccr.clearV
    op3 & 0x80 match {
      case 0x00 =>
        //MOV.L @Reg+,Reg [01][00][6D][pregreg]
        val mov = data.mem.getLong(ind)
        data.reg.setLong(mov, op3)
        data.reg.setLong(ind + 4, op3 >> 4)
        check2(mov, 32, data)

      case 0x80 =>
        //MOV.L Reg,@-Reg [01][00][6D][1pregreg]
        val mov = data.reg.getLong(op3)
        data.mem.setLong(mov, ind - 4)
        data.reg.setLong(ind - 4, op3 >> 4)
        check2(mov, 32, data)
    }
  }

  private def analyze01006F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val disp = rom.getWord(pc + 2)
    val ind = data.reg.getLong(op3 >> 4) + disp
    data.ccr.clearV
    data.pc.setPc(pc + 6)
    op3 & 0x80 match {
      case 0x00 =>
        //MOV.L DIsp16,Reg [01][00][6F][dregreg][disp][disp]
        val mov = data.mem.getLong(ind)
        data.reg.setLong(mov, op3)
        check2(mov, 32, data)

      case 0x80 =>
        //MOV.L Reg,Disp16 [01][00][6F][1dregreg][disp][disp]
        val mov = data.reg.getLong(op3)
        data.mem.setLong(mov, ind)
        check2(mov, 32, data)
    }
  }

  private def analyze010078(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val op5 = rom.getByte(pc + 5)
    val disp = rom.getLong(pc + 6)
    val ind = data.reg.getLong(op3 >> 4) + disp
    data.pc.setPc(pc + 10)
    data.ccr.clearV
    op3 & 0x80 match {
      case 0x00 =>
        //MOV.L Disp24,Reg [01][00][78][dreg0][6B][2reg][00][disp][disp][disp]
        val mov = data.mem.getLong(ind)
        data.reg.setLong(mov, op5)
        check2(mov, 32, data)

      case 0x80 =>
        //MOV.L Reg,Disp24 [01][00][78][1dreg0][6B][Areg][00][disp][disp][disp]
        val mov = data.reg.getLong(op5)
        data.mem.setLong(mov, ind)
        check2(mov, 32, data)
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
    val ind = data.reg.getLong(op3 >> 4)
    data.pc.setPc(pc + 4)
    op3 & 0x80 match {
      case 0x00 =>
        //LDC.W IndReg,CCR [01][40][69][reg0]
        data.ccr.setCcr(data.mem.getWord(ind).extract(7, 0))
        ArrayBuffer(data)

      case 0x80 =>
        //STC.W CCR,IndReg [01][40][69][1reg0]
        data.mem.setByte(data.ccr.getCcr, ind)
        ArrayBuffer(data)
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
        data.ccr.setCcr(data.mem.getByte(abs))

      case 0x80 =>
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
        data.mem.setByte(data.ccr.getCcr, abs)
    }
    ArrayBuffer(data)
  }

  private def analyze01406D(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val ind = data.reg.getLong(op3 >> 4)
    data.pc.setPc(pc + 4)
    op3 & 0x80 match {
      case 0x00 =>
        //LDC.W @Reg+,CCR [01][40][6D][preg0]
        data.ccr.setCcr(data.mem.getLong(ind))
        data.reg.setLong(ind + 4, op3 >> 4)
        ArrayBuffer(data)

      case 0x80 =>
        //STC.W CCR,@-Reg [01][40][6D][1preg0]
        data.mem.setByte(data.ccr.getCcr, ind - 4)
        data.reg.setLong(ind - 4, op3 >> 4)
        ArrayBuffer(data)
    }
  }

  private def analyze01406F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val disp = rom.getWord(pc + 2)
    val ind = data.reg.getLong(op3 >> 4) + disp
    data.pc.setPc(pc + 6)
    op3 & 0x80 match {
      case 0x00 =>
        //LDC.W DIsp16,CCR [01][40][6F][dreg0][disp][disp]
        data.ccr.setCcr(data.mem.getByte(ind))
        ArrayBuffer(data)

      case 0x80 =>
        //STC.W CCR,Disp16 [01][40][6F][1dreg0][disp][disp]
        data.mem.setByte(data.ccr.getCcr, ind)
        ArrayBuffer(data)
    }
  }

  private def analyze014078(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val disp = rom.getLong(pc + 6)
    val ind = data.reg.getLong(op3 >> 4) + disp
    data.pc.setPc(pc + 10)
    op3 & 0x80 match {
      case 0x00 =>
        //STC.W Disp24,CCR [01][40][78][dreg0][6B][20][00][disp][disp][disp]
        data.ccr.setCcr(data.mem.getByte(ind))
        ArrayBuffer(data)

      case 0x80 =>
        //MOV.L Reg,Disp24 [01][00][78][1dreg0][6B][Areg][00][disp][disp][disp]
        data.mem.setByte(data.ccr.getCcr, ind)
        ArrayBuffer(data)
    }
  }

  private def analyze01C(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    data.pc.setPc(pc + 4)
    rom.getByte(pc + 2) match {
      case 0x50 =>
        //MULXS.B regB,regW [01][C0][50][regBregW]
        val regW = data.reg.getWord(op3).extract(7, 0).sextend(8)
        val regB = data.reg.getByte(op3 >> 4).sextend(8)
        val mul = regW * regB
        data.reg.setWord(mul, op3)
        check2(mul, 16, data)

      case 0x52 =>
        //MULXS.W regW,regL [01][C0][50][regWregL]
        val regL = data.reg.getLong(op3).extract(31, 16).sextend(16)
        val regW = data.reg.getWord(op3 >> 4).sextend(16)
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
        val regB = data.reg.getByte(op3 >> 4).sextend(8)
        val div = regW sdiv regB
        val rem = regW srem regB
        val ans = rem.extract(7, 0) concat div.extract(7, 0)
        data.reg.setWord(ans, op3)
        checkN(div.extract(7, 0), checkZ(regB, ArrayBuffer(data), 8), 8)

      case 0x52 =>
        //DIVXS.W regW,regL [01][D0][52][regWregL]
        val regL = data.reg.getLong(op3)
        val regW = data.reg.getWord(op3 >> 4).sextend(16)
        val div = regL sdiv regW
        val rem = regL srem regW
        val ans = rem.extract(15, 0) concat div.extract(15, 0)
        data.reg.setLong(ans, op3)
        checkN(div.extract(15, 0), checkZ(regW, ArrayBuffer(data), 16), 16)
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
            check3(reg, new CtxSymbol(1, 8), inc, 8, data)
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
        check3(reg, new CtxSymbol(1, 16), inc, 16, data)

      case 0x70 =>
        //INC.L #1,Reg [0B][7reg]
        val reg = data.reg.getLong(op1)
        val inc = reg + 1
        data.reg.setLong(inc, op1)
        check3(reg, new CtxSymbol(1, 32), inc, 32, data)

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
        check3(reg, new CtxSymbol(2, 16), inc, 16, data)

      case 0xF0 =>
        //INC.L #2,Reg [0B][Freg]
        val reg = data.reg.getLong(op1)
        val inc = reg + 2
        data.reg.setLong(inc, op1)
        check3(reg, new CtxSymbol(2, 32), inc, 32, data)
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
        val and = regB | regA
        data.reg.setByte(and, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(and, 8, data)

      case 0x15 =>
        //XOR.B RegA,RegB [15][regAregB]
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val and = regB ^ regA
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

      case 0x1A => analyze1A(data, pc)
      case 0x1B => analyze1B(data, pc)

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
        val carry = data.ccr.getCcr.extract(0, 0).zextend(7)
        val imm = regA + carry
        val add = regB - imm
        data.reg.setByte(add, op1)
        check5(regB, imm.neg, add, 8, data)

      case 0x1F => analyze1F(data, pc)
    }
  }

  private def analyze10(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 2)
    op1 & 0xF0 match {
      case 0x00 =>
        //SHLL.B Reg [10][0reg]
        val reg = data.reg.getByte(op1)
        val shift = reg << 1
        data.reg.setByte(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(7, 7))
        check2(shift, 8, data)

      case 0x01 =>
        //SHLL.W Reg [10][1reg]
        val reg = data.reg.getWord(op1)
        val shift = reg << 1
        data.reg.setWord(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(15, 15))
        check2(shift, 16, data)

      case 0x03 =>
        //SHLL.L Reg [10][3reg]
        val reg = data.reg.getLong(op1)
        val shift = reg << 1
        data.reg.setLong(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(31, 31))
        check2(shift, 32, data)

      case 0x80 =>
        //SHAL.B Reg [10][8reg]
        val reg = data.reg.getByte(op1)
        val shift = reg << 1
        data.reg.setByte(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(7, 7))
        check3(reg, reg, shift, 8, data)

      case 0x90 =>
        //SHAL.W Reg [10][9reg]
        val reg = data.reg.getWord(op1)
        val shift = reg << 1
        data.reg.setWord(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(15, 15))
        check3(reg, reg, shift, 16, data)

      case 0xB0 =>
        //SHAL.L Reg [10][Breg]
        val reg = data.reg.getLong(op1)
        val shift = reg << 1
        data.reg.setLong(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(31, 31))
        check3(reg, reg, shift, 32, data)
    }
  }

  private def analyze11(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.pc.setPc(pc + 2)
    data.ccr.clearV
    val ccr = data.ccr.getCcr
    op1 & 0xF0 match {
      case 0x00 =>
        //SHLR.B Reg [11][0reg]
        val reg = data.reg.getByte(op1)
        val shift = new CtxSymbol(ctx.mkBVLshr(reg.symbol, ctx.mkInt(1, reg.symbol.getSort)))
        data.reg.setByte(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        data.ccr.clearN
        checkZ(shift, ArrayBuffer(data), 8)

      case 0x01 =>
        //SHLR.W Reg [11][1reg]
        val reg = data.reg.getWord(op1)
        val shift = new CtxSymbol(ctx.mkBVLshr(reg.symbol, ctx.mkInt(1, reg.symbol.getSort)))
        data.reg.setWord(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        data.ccr.clearN
        checkZ(shift, ArrayBuffer(data), 16)

      case 0x03 =>
        //SHLR.L Reg [10][3reg]
        val reg = data.reg.getLong(op1)
        val shift = new CtxSymbol(ctx.mkBVLshr(reg.symbol, ctx.mkInt(1, reg.symbol.getSort)))
        data.reg.setLong(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        data.ccr.clearN
        checkZ(shift, ArrayBuffer(data), 32)

      case 0x80 =>
        //SHAR.B Reg [10][8reg]
        val reg = data.reg.getByte(op1)
        val shift = reg >> 1
        data.reg.setByte(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        check2(shift, 8, ArrayBuffer(data))

      case 0x90 =>
        //SHAR.W Reg [10][9reg]
        val reg = data.reg.getWord(op1)
        val shift = reg >> 1
        data.reg.setWord(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        check2(shift, 16, ArrayBuffer(data))

      case 0xB0 =>
        //SHAR.L Reg [10][Breg]
        val reg = data.reg.getLong(op1)
        val shift = reg >> 1
        data.reg.setLong(shift, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        check2(shift, 32, ArrayBuffer(data))
    }
  }

  private def analyze12(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.ccr.clearV
    val ccr = data.ccr.getCcr
    op1 & 0xF0 match {
      case 0x00 =>
        //ROTXL.B Reg [12][0reg]
        val reg = data.reg.getByte(op1)
        val rotate = reg.extract(6, 0) concat ccr.extract(0, 0)
        data.reg.setByte(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(7, 7))
        check2(rotate, 8, data)

      case 0x10 =>
        //ROTXL.W Reg [12][1reg]
        val reg = data.reg.getWord(op1)
        val rotate = reg.extract(14, 0) concat ccr.extract(0, 0)
        data.reg.setWord(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(15, 15))
        check2(rotate, 16, data)

      case 0x30 =>
        //ROTXL.L Reg [12][3reg]
        val reg = data.reg.getLong(op1)
        val rotate = reg.extract(30, 0) concat ccr.extract(0, 0)
        data.reg.setLong(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(31, 31))
        check2(rotate, 32, data)

      case 0x80 =>
        //ROTL.B Reg [12][8reg]
        val reg = data.reg.getByte(op1)
        val rotate = reg.extract(6, 0) concat reg.extract(7, 7)
        data.reg.setByte(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(7, 7))
        check2(rotate, 8, data)

      case 0x90 =>
        //ROTL.W Reg [12][9reg]
        val reg = data.reg.getWord(op1)
        val rotate = reg.extract(14, 0) concat reg.extract(15, 15)
        data.reg.setWord(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(15, 15))
        check2(rotate, 16, data)

      case 0xB0 =>
        //ROTL.L Reg [12][Breg]
        val reg = data.reg.getLong(op1)
        val rotate = reg.extract(30, 0) concat reg.extract(31, 31)
        data.reg.setLong(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(31, 31))
        check2(rotate, 32, data)
    }
  }

  private def analyze13(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.ccr.clearV
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 2)
    op1 & 0xF0 match {
      case 0x00 =>
        //ROTXR.B Reg [13][0reg]
        val reg = data.reg.getByte(op1)
        val rotate = ccr.extract(0, 0) concat reg.extract(7, 1)
        data.reg.setByte(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        check2(rotate, 8, data)

      case 0x10 =>
        //ROTXR.W Reg [13][1reg]
        val reg = data.reg.getWord(op1)
        val rotate = ccr.extract(0, 0) concat reg.extract(15, 1)
        data.reg.setWord(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        check2(rotate, 16, data)

      case 0x30 =>
        //ROTXR.L Reg [13][3reg]
        val reg = data.reg.getLong(op1)
        val rotate = ccr.extract(0, 0) concat reg.extract(31, 1)
        data.reg.setLong(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        check2(rotate, 32, data)

      case 0x80 =>
        //ROTR.B Reg [13][8reg]
        val reg = data.reg.getByte(op1)
        val rotate = reg.extract(0, 0) concat reg.extract(7, 1)
        data.reg.setByte(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        check2(rotate, 8, data)

      case 0x90 =>
        //ROTR.W Reg [13][9reg]
        val reg = data.reg.getWord(op1)
        val rotate = reg.extract(0, 0) concat reg.extract(15, 1)
        data.reg.setWord(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        check2(rotate, 16, data)

      case 0xB0 =>
        //ROTR.L Reg [13][Breg]
        val reg = data.reg.getLong(op1)
        val rotate = reg.extract(0, 0) concat reg.extract(31, 1)
        data.reg.setLong(rotate, op1)
        data.ccr.setCcr(ccr.extract(7, 1) concat reg.extract(0, 0))
        check2(rotate, 32, data)
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
        val extu = data.reg.getWord(op1).extract(7, 0).zextend(8)
        data.reg.setWord(extu, op1)
        data.ccr.clearN
        data.ccr.clearV
        checkZ(extu, data, 16)

      case 0x70 =>
        //EXTU.L Reg [17][7reg]
        val extu = data.reg.getLong(op1).extract(15, 0).zextend(16)
        data.reg.setLong(extu, op1)
        data.ccr.clearN
        data.ccr.clearV
        checkZ(extu, data, 32)

      case 0x80 =>
        //NEG.B Reg [17][8reg]
        val reg = data.reg.getByte(op1)
        val neg = reg.neg
        data.reg.setByte(neg, op1)
        check5(new CtxSymbol(0, 8), reg.neg, neg, 8, data)

      case 0x90 =>
        //NEG.B Reg [17][9reg]
        val reg = data.reg.getWord(op1)
        val neg = reg.neg
        data.reg.setWord(neg, op1)
        check5(new CtxSymbol(0, 8), reg.neg, neg, 16, data)

      case 0xB0 =>
        //NEG.B Reg [17][Breg]
        val reg = data.reg.getLong(op1)
        val neg = reg.neg
        data.reg.setLong(neg, op1)
        check5(new CtxSymbol(0, 8), reg.neg, neg, 32, data)

      case 0xD0 =>
        //EXTS.W Reg [17][Dreg]
        val exts = data.reg.getByte(op1).sextend(8)
        data.reg.setWord(exts, op1)
        data.ccr.clearV
        check2(exts, 16, data)

      case 0xF0 =>
        //EXTS.L Reg [17][Freg]
        val exts = data.reg.getWord(op1).sextend(16)
        data.reg.setLong(exts, op1)
        data.ccr.clearV
        check2(exts, 32, data)
    }
  }

  private def analyze1A(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.pc.setPc(pc + 2)
    op1 & 0x80 match {
      case 0x00 =>
        op1 & 0xF0 match {
          case 0x00 =>
            //DEC.B Reg [1A][0reg]
            val reg = data.reg.getByte(op1)
            val dec = reg - 1
            data.reg.setByte(dec, op1)
            data.pc.setPc(pc + 2)
            check3(reg, new CtxSymbol(1, 8).neg, dec, 8, data)
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

  private def analyze1B(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.pc.setPc(pc + 2)
    op1 & 0xF0 match {
      case 0x00 =>
        //SUBS.L #1,Reg [1B][0reg]
        val sub = data.reg.getLong(op1) - 1
        data.reg.setLong(sub, op1)
        ArrayBuffer(data)

      case 0x50 =>
        //DEC.W #1,Reg [1B][5reg]
        val reg = data.reg.getWord(op1)
        val dec = reg - 1
        data.reg.setWord(dec, op1)
        check3(reg, new CtxSymbol(1, 16).neg, dec, 16, data)

      case 0x70 =>
        //DEC.L #1,Reg [1B][7reg]
        val reg = data.reg.getLong(op1)
        val dec = reg - 1
        data.reg.setLong(dec, op1)
        check3(reg, new CtxSymbol(1, 32).neg, dec, 32, data)

      case 0x80 =>
        //SUBS.L #2,Reg [1B][8reg]
        val sub = data.reg.getLong(op1) - 2
        data.reg.setLong(sub, op1)
        ArrayBuffer(data)

      case 0x90 =>
        //SUBS.L #4,Reg [1B][9reg]
        val sub = data.reg.getLong(op1) - 4
        data.reg.setLong(sub, op1)
        ArrayBuffer(data)

      case 0xD0 =>
        //DEC.W #2,Reg [1B][Dreg]
        val reg = data.reg.getWord(op1)
        val dec = reg - 2
        data.reg.setWord(dec, op1)
        check3(reg, new CtxSymbol(2, 16).neg, dec, 16, data)

      case 0xF0 =>
        //DEC.L #2,Reg [1B][Freg]
        val reg = data.reg.getLong(op1)
        val inc = reg - 2
        data.reg.setLong(inc, op1)
        check3(reg, new CtxSymbol(2, 32).neg, inc, 32, data)
    }
  }

  private def analyze1F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    data.pc.setPc(pc + 2)
    op1 & 0xF0 match {
      //case 0x00 => DAS.B Reg [1F][0reg]

      case _ =>
        //CMP.L RegA,RegB [0F][1regAregB]
        val regA = data.reg.getLong(op1 >> 1)
        val regB = data.reg.getLong(op1)
        val cmp = regB - regA
        data.reg.setLong(cmp, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA.neg, cmp, 32, data)
    }
  }

  private def analyze4(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val disp = op1 | (if ((op1 & 0x80) == 0x80) 0xFFFFFF00 else 0)
    val ccr = data.ccr.getCcr
    rom.getByte(pc) match {
      //Bcc [4X][disp]
      case 0x40 =>
        //BRA true
        data.pc.setPc(pc + disp)
        ArrayBuffer(data)

      case 0x41 =>
        //BRN
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x42 =>
        //BHI C|V = 0
        val c = ccr.extract(0, 0)
        val z = ccr.extract(2, 2)
        val clone = twoPathClone((c | z).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x43 =>
        //BLS C|V = 1
        val c = ccr.extract(0, 0)
        val z = ccr.extract(2, 2)
        val clone = twoPathClone((c | z).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x44 =>
        //BHS C = 0
        val clone = twoPathClone(ccr.extract(0, 0).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x45 =>
        //BLO C = 1
        val clone = twoPathClone(ccr.extract(0, 0).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x46 =>
        //BNE Z = 0
        val clone = twoPathClone(ccr.extract(2, 2).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x47 =>
        //BEQ Z = 1
        val clone = twoPathClone(ccr.extract(2, 2).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x48 =>
        //BVC V = 0
        val clone = twoPathClone(ccr.extract(1, 1).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x49 =>
        //BVC V = 1
        val clone = twoPathClone(ccr.extract(1, 1).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x4A =>
        //BPL N = 0
        val clone = twoPathClone(ccr.extract(3, 3).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x4B =>
        //BMI N = 0
        val clone = twoPathClone(ccr.extract(3, 3).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x4C =>
        //BGE N 排他的論理和 V = 0
        val n = ccr.extract(3, 3)
        val v = ccr.extract(1, 1)
        val clone = twoPathClone((n ^ v).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x4D =>
        //BLT N 排他的論理和 V = 1
        val n = ccr.extract(3, 3)
        val v = ccr.extract(1, 1)
        val clone = twoPathClone((n ^ v).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x4E =>
        //BGT Z | (N 排他的論理和 V) = 0
        val n = ccr.extract(3, 3)
        val z = ccr.extract(2, 2)
        val v = ccr.extract(1, 1)
        val clone = twoPathClone((z | (n ^ v)).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)

      case 0x4F =>
        //BLE Z | (N 排他的論理和 V) = 1
        val n = ccr.extract(3, 3)
        val z = ccr.extract(2, 2)
        val v = ccr.extract(1, 1)
        val clone = twoPathClone((z | (n ^ v)).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 2)
        tapleToArray(clone)
    }
  }

  private def analyze5(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc) match {
      case 0x50 =>
        //MULXU.B regB,regW [50][regBregW]
        val op1 = rom.getByte(pc + 1)
        val regW = data.reg.getWord(op1).extract(7, 0).sextend(8)
        val regB = data.reg.getByte(op1 >> 4).sextend(8)
        data.reg.setWord(regW * regB, op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x51 =>
        //DIVXU.B regB,regW [51][regBregW]
        val op1 = rom.getByte(pc + 1)
        val regW = data.reg.getWord(op1)
        val regB = data.reg.getByte(op1 >> 4).sextend(8)
        val ans = (regW srem regB).extract(7, 0) concat (regW sdiv regB).extract(7, 0)
        data.reg.setWord(ans, op1)
        data.pc.setPc(pc + 2)
        check2(regB, 8, data)

      case 0x52 =>
        //MULXU.L regB,regW [52][regBregW]
        val op1 = rom.getByte(pc + 1)
        val regL = data.reg.getLong(op1).extract(15, 0).sextend(16)
        val regW = data.reg.getWord(op1 >> 4).sextend(16)
        data.reg.setWord(regL * regW, op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x53 =>
        //DIVXU.W regW,regL [53][regWregL]
        val op1 = rom.getByte(pc + 1)
        val regL = data.reg.getLong(op1)
        val regW = data.reg.getWord(op1 >> 4).sextend(16)
        val ans = (regL srem regW).extract(7, 0) concat (regL sdiv regW).extract(7, 0)
        data.reg.setWord(ans, op1)
        data.pc.setPc(pc + 2)
        check2(regW, 8, data)

      case 0x54 =>
        //RTS [54][70]
        val sp = data.reg.getLong(7)
        data.reg.setLong(sp + 2, 7)
        val mem = data.mem.getWord(sp)
        val rts = data.pc.setPc(mem)
        if (rts == rom.getWord(0) + 10) data.stop = true
        ArrayBuffer(data)

      case 0x55 =>
        //BSR disp [55][disp]
        val op1 = rom.getByte(pc + 1)
        val disp = op1 | (if ((op1 & 0x80) == 0x80) 0xFFFFFF00 else 0)
        val sp = data.reg.getLong(7) - 2
        data.reg.setLong(sp, 7)
        data.pc.setPc(pc + disp)
        data.mem.setWord(pc + 2, sp)
        ArrayBuffer(data)

      case 0x56 =>
        //RTE [56][70]
        val sp = data.reg.getLong(7)
        data.reg.setLong(sp + 4, 7)
        val mem = data.mem.getLong(sp)
        data.ccr.setCcr(mem.extract(31, 24))
        data.pc.setPc(mem)
        ArrayBuffer(data)

      //case 0x57 => //TRAPA Imm

      case 0x58 => analyze58(data, pc)

      case 0x59 =>
        //JMP IndReg [59][reg0]
        val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
        data.pc.setPc(ind)
        ArrayBuffer(data)

      case 0x5A =>
        //JMP Abs [5A][abs][abs][abs]
        data.pc.setPc(rom.getLong(pc))
        ArrayBuffer(data)

      case 0x5B =>
        //JMP IndAbs [5B][abs]
        val ind = data.mem.getWord(rom.getByte(pc + 1))
        data.pc.setPc(ind)
        ArrayBuffer(data)

      case 0x5C =>
        //BSR disp [5C][00][disp][disp]
        val disp = rom.getByte(pc + 1)
        val sp = data.reg.getLong(7) - 2
        data.reg.setLong(sp, 7)
        data.pc.setPc(pc + disp)
        data.mem.setWord(new CtxSymbol(pc + 4, 16), sp)
        ArrayBuffer(data)

      case 0x5D =>
        //JSR IndReg [59][reg0]
        val sp = data.reg.getLong(7) - 2
        data.reg.setLong(sp, 7)
        data.mem.setWord(new CtxSymbol(pc + 2, 16), sp)
        val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
        data.pc.setPc(ind)
        ArrayBuffer(data)

      case 0x5E =>
        //JSR Abs:24 [5E][Abs][Abs][Abs]
        val sp = data.reg.getLong(7) - 2
        data.reg.setLong(sp, 7)
        data.pc.setPc(rom.getLong(pc))
        data.mem.setWord(new CtxSymbol(pc + 4, 16), sp)
        ArrayBuffer(data)

      case 0x5F =>
        //JSR IndReg [5F][abs]
        val sp = data.reg.getLong(7) - 2
        data.reg.setLong(sp, 7)
        data.mem.setWord(new CtxSymbol(pc + 2, 16), sp)
        val ind = data.mem.getWord(rom.getByte(pc + 1))
        data.pc.setPc(ind)
        ArrayBuffer(data)
    }
  }

  private def analyze58(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val disp = rom.getWord(pc + 2)
    val ccr = data.ccr.getCcr
    rom.getByte(pc + 1) match {
      //Bcc [58][X0][disp][disp]
      case 0x00 =>
        //BRA true
        data.pc.setPc(pc + disp)
        ArrayBuffer(data)

      case 0x10 =>
        //BRN
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x20 =>
        //BHI C|V = 0
        val c = ccr.extract(0, 0)
        val z = ccr.extract(2, 2)
        val clone = twoPathClone((c | z).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0x30 =>
        //BLS C|V = 1
        val c = ccr.extract(0, 0)
        val z = ccr.extract(2, 2)
        val clone = twoPathClone((c | z).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0x40 =>
        //BHS C = 0
        val clone = twoPathClone(ccr.extract(0, 0).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0x50 =>
        //BLO C = 1
        val clone = twoPathClone(ccr.extract(0, 0).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0x60 =>
        //BNE Z = 0
        val clone = twoPathClone(ccr.extract(2, 2).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0x70 =>
        //BEQ Z = 1
        val clone = twoPathClone(ccr.extract(2, 2).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0x80 =>
        //BVC V = 0
        val clone = twoPathClone(ccr.extract(1, 1).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0x90 =>
        //BVC V = 1
        val clone = twoPathClone(ccr.extract(1, 1).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0xA0 =>
        //BPL N = 0
        val clone = twoPathClone(ccr.extract(3, 3).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0xB0 =>
        //BMI N = 0
        val clone = twoPathClone(ccr.extract(3, 3).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0xC0 =>
        //BGE N 排他的論理和 V = 0
        val n = ccr.extract(3, 3)
        val v = ccr.extract(1, 1)
        val clone = twoPathClone((n ^ v).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0xD0 =>
        //BLT N 排他的論理和 V = 1
        val n = ccr.extract(3, 3)
        val v = ccr.extract(1, 1)
        val clone = twoPathClone((n ^ v).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0xE0 =>
        //BGT Z | (N 排他的論理和 V) = 0
        val n = ccr.extract(3, 3)
        val z = ccr.extract(2, 2)
        val v = ccr.extract(1, 1)
        val clone = twoPathClone((z | (n ^ v)).equal(0), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)

      case 0xF0 =>
        //BLE Z | (N 排他的論理和 V) = 1
        val n = ccr.extract(3, 3)
        val z = ccr.extract(2, 2)
        val v = ccr.extract(1, 1)
        val clone = twoPathClone((z | (n ^ v)).equal(1), data)
        clone._1.pc.setPc(pc + disp)
        clone._2.pc.setPc(pc + 4)
        tapleToArray(clone)
    }
  }

  private def analyze6(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    rom.getByte(pc) match {
      case 0x60 =>
        //BSET.B RegA,RegB [60][regAregB]
        val imm = data.reg.getByte(op1 >> 4) & 0x07
        val bset = data.reg.getByte(op1).bitSet(imm)
        data.reg.setByte(bset, op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x61 =>
        //BNOT.B RegA,RegB [61][regAregB]
        val imm = data.reg.getByte(op1 >> 4) & 0x07
        val bnot = data.reg.getByte(op1).bitNot(imm)
        data.reg.setByte(bnot, op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x62 =>
        //BCLR.B RegA,RegB [62][regAregB]
        val imm = data.reg.getByte(op1 >> 4) & 0x07
        val bclr = data.reg.getByte(op1).bitClr(imm)
        data.reg.setByte(bclr, op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x63 =>
        //BTST.B RegA,RegB [63][regAregB]
        val imm = data.reg.getByte(op1 >> 4) & 0x07
        val reg = data.reg.getByte(op1).bitGet(imm).~
        val ccr = data.ccr.getCcr
        data.ccr.setCcr(ccr.extract(7, 3) concat reg concat ccr.extract(1, 0))
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x64 =>
        //OR.W RegA,RegB [64][regAregB]
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val or = regB | regA
        data.reg.setWord(or, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(or, 16, data)

      case 0x65 =>
        //XOR.W RegA,RegB [65][regAregB]
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val xor = regB ^ regA
        data.reg.setWord(xor, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(xor, 16, data)

      case 0x66 =>
        //AND.W RegA,RegB [66][regAregB]
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val and = regB & regA
        data.reg.setWord(and, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(and, 16, data)

      case 0x67 => analyze67(data, pc)
      case 0x68 => analyze68(data, pc)
      case 0x69 => analyze69(data, pc)
      case 0x6A => analyze6A(data, pc)
      case 0x6B => analyze6B(data, pc)
      case 0x6C => analyze6C(data, pc)
      case 0x6D => analyze6D(data, pc)
      case 0x6E => analyze6E(data, pc)
      case 0x6F => analyze6F(data, pc)
    }
  }

  private def analyze67(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val imm = (op1 >> 4) & 0x07
    val reg = data.reg.getByte(op1)
    val c = data.ccr.getCcr.extract(0, 0)
    data.pc.setPc(pc + 2)
    op1 & 0x80 match {
      case 0x00 =>
        //BST.B Imm,Reg [67][imm reg]
        data.reg.setByte(reg.bitStore(c, imm), op1)

      case 0x80 =>
        //BIST.B Imm,Reg [67][1imm reg]
        data.reg.setByte(reg.bitStore(c.~, imm), op1)
    }
    ArrayBuffer(data)
  }

  private def analyze68(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val ind = data.reg.getLong(op1 >> 4)
    data.pc.setPc(pc + 2)
    data.ccr.clearV
    op1 & 0x80 match {
      case 0x00 =>
        //MOV.B IndReg,Reg [68][0indreg reg]
        val mov = data.mem.getByte(ind)
        data.reg.setByte(mov, op1)
        check2(mov, 8, data)

      case 0x80 =>
        //MOV,B Reg,IndReg [68][1indreg reg]
        val mov = data.reg.getByte(op1)
        data.mem.setByte(mov, ind)
        check2(mov, 8, data)
    }
  }

  private def analyze69(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val ind = data.reg.getLong(op1 >> 4)
    data.pc.setPc(pc + 2)
    data.ccr.clearV
    op1 & 0x80 match {
      case 0x00 =>
        //MOV.W IndReg,Reg [69][0indreg reg]
        val mov = data.mem.getWord(ind)
        data.reg.setWord(mov, op1)
        check2(mov, 16, data)

      case 0x80 =>
        //MOV,W Reg,IndReg [69][1indreg reg]
        val mov = data.reg.getWord(op1)
        data.mem.setWord(mov, ind)
        check2(mov, 16, data)
    }
  }

  private def analyze6A(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
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
  }

  private def analyze6B(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
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
  }

  private def analyze6C(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val ind = data.reg.getLong(op1 >> 4)
    data.pc.setPc(pc + 2)
    data.ccr.clearV
    op1 & 0x80 match {
      case 0x00 =>
        //MOV.B @Reg+,Reg [6C][pregreg]
        val mov = data.mem.getByte(ind)
        data.reg.setByte(mov, op1)
        data.reg.setLong(ind + 1, op1 >> 4)
        check2(mov, 8, data)

      case 0x80 =>
        //MOV.B Reg,@-Reg [6C][1pregreg]
        val mov = data.reg.getByte(op1)
        data.mem.setByte(mov, ind - 1)
        data.reg.setLong(ind - 1, op1 >> 4)
        check2(mov, 8, data)
    }
  }

  private def analyze6D(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val ind = data.reg.getLong(op1 >> 4)
    data.pc.setPc(pc + 2)
    data.ccr.clearV
    op1 & 0x80 match {
      case 0x00 =>
        //MOV.W @Reg+,Reg [6D][pregreg]
        val mov = data.mem.getWord(ind)
        data.reg.setWord(mov, op1)
        data.reg.setLong(ind + 2, op1 >> 4)
        check2(mov, 16, data)

      case 0x80 =>
        //MOV.W Reg,@-Reg [6D][1pregreg]
        val mov = data.reg.getWord(op1)
        data.mem.setWord(mov, ind - 2)
        data.reg.setLong(ind - 2, op1 >> 4)
        check2(mov, 16, data)
    }
  }

  private def analyze6E(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val disp = rom.getWord(pc + 2)
    val ind = data.reg.getLong(op1 >> 4) + disp
    data.pc.setPc(pc + 4)
    data.ccr.clearV
    op1 & 0x80 match {
      case 0x00 =>
        //MOV.B Disp,Reg [6E][dregreg][disp][disp]
        val mov = data.mem.getByte(ind)
        data.reg.setByte(mov, op1)
        check2(mov, 8, data)

      case 0x80 =>
        //MOV.B Reg,Disp [6E][dregreg][disp][disp]
        val mov = data.reg.getByte(op1)
        data.mem.setByte(mov, ind)
        check2(mov, 8, data)
    }
  }

  private def analyze6F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val disp = rom.getWord(pc + 2)
    val ind = data.reg.getLong(op1 >> 4) + disp
    data.pc.setPc(pc + 4)
    data.ccr.clearV
    op1 & 0x80 match {
      case 0x00 =>
        //MOV.W Disp,Reg [6F][dregreg][disp][disp]
        val mov = data.mem.getWord(ind)
        data.reg.setWord(mov, op1)
        check2(mov, 16, data)

      case 0x80 =>
        //MOV.W Reg,Disp [6F][dregreg][disp][disp]
        val mov = data.reg.getWord(op1)
        data.mem.setWord(mov, ind)
        check2(mov, 16, data)
    }
  }

  private def analyze7(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val imm = op1 >> 4
    val reg = data.reg.getByte(op1)
    rom.getByte(pc) match {
      case 0x70 =>
        //BSET.B Imm,Reg [70][immreg]
        data.reg.setByte(reg.bitSet(imm), op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x71 =>
        //BNOT.B Imm,Reg [71][immreg]
        data.reg.setByte(reg.bitNot(imm), op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x72 =>
        //BCLR.B Imm,Reg [72][immreg]
        data.reg.setByte(reg.bitClr(imm), op1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x73 =>
        //BTST.B Imm,Reg [73][immreg]
        val ccr = data.ccr.getCcr
        data.ccr.setCcr(ccr.extract(7, 3) concat (reg.bitGet(imm).~) concat ccr.extract(1, 0))
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x74 => analyze74(data, pc)
      case 0x75 => analyze75(data, pc)
      case 0x76 => analyze76(data, pc)
      case 0x77 => analyze77(data, pc)
      case 0x78 => analyze78(data, pc)
      case 0x79 => analyze79(data, pc)
      case 0x7A => analyze7A(data, pc)
      case 0x7B => analyze7B(data, pc)
      case 0x7C => analyze7C(data, pc)
      case 0x7D => analyze7D(data, pc)
      case 0x7E => analyze7E(data, pc)
      case 0x7F => analyze7F(data, pc)
    }
  }

  private def analyze74(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val imm = (op1 >> 4) & 0x07
    val reg = data.reg.getByte(op1).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 2)
    val and = op1 & 0x80 match {
      case 0x00 =>
        //BOR.B Imm,Reg [74][0imm reg]
        ccr.extract(0, 0) | reg

      case 0x80 =>
        //BIOR.B Imm,Reg [74][1imm reg]
        ccr.extract(0, 0) | (reg.~)
    }
    data.ccr.setCcr(ccr.extract(7, 1) concat and)
    ArrayBuffer(data)
  }

  private def analyze75(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val imm = (op1 >> 4) & 0x07
    val reg = data.reg.getByte(op1).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 2)
    val and = op1 & 0x80 match {
      case 0x00 =>
        //BXOR.B Imm,Reg [75][0imm reg]
        ccr.extract(0, 0) ^ reg

      case 0x80 =>
        //BIXOR.B Imm,Reg [75][1imm reg]
        ccr.extract(0, 0) ^ (reg.~)
    }
    data.ccr.setCcr(ccr.extract(7, 1) concat and)
    ArrayBuffer(data)
  }

  private def analyze76(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val imm = (op1 >> 4) & 0x07
    val reg = data.reg.getByte(op1).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 2)
    val and = op1 & 0x80 match {
      case 0x00 =>
        //BAND.B Imm,Reg [76][0imm reg]
        ccr.extract(0, 0) & reg

      case 0x80 =>
        //BIAND.B Imm,Reg [76][1imm reg]
        ccr.extract(0, 0) & (reg.~)
    }
    data.ccr.setCcr(ccr.extract(7, 1) concat and)
    ArrayBuffer(data)
  }

  private def analyze77(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val imm = (op1 >> 4) & 0x07
    val reg = data.reg.getByte(op1).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 2)
    op1 & 0x80 match {
      case 0x00 =>
        //BLD.B Imm,Reg [77][immreg]
        data.ccr.setCcr(ccr.extract(7, 1) concat reg)

      case 0x80 =>
        //BILD.B Imm,Reg [77][1immreg]
        data.ccr.setCcr(ccr.extract(7, 1) concat reg)
    }
    ArrayBuffer(data)
  }

  private def analyze78(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val disp = rom.getLong(pc + 4)
    val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4) + disp
    data.pc.setPc(pc + 8)
    data.ccr.clearV
    rom.getByte(pc + 2) match {
      case 0x6A =>
        op3 & 0xF0 match {
          case 0x20 =>
            //MOV.B Disp,Reg [78][dreg0][6A][2reg][00][disp][disp][disp]
            val mov = data.mem.getByte(ind)
            data.reg.setByte(mov, op3)
            check2(mov, 8, data)

          case 0xA0 =>
            //MOV.B Reg,Disp [78][dreg0][6A][Areg][00][disp][disp][disp]
            val mov = data.reg.getByte(op3)
            data.mem.setByte(mov, ind)
            check2(mov, 8, data)
        }

      case 0x6B =>
        op3 & 0xF0 match {
          case 0x20 =>
            //MOV.W Disp,Reg [78][dreg0][6B][2reg][00][disp][disp][disp]
            val mov = data.mem.getWord(ind)
            data.reg.setWord(mov, op3)
            check2(mov, 16, data)

          case 0xA0 =>
            //MOV.W Reg,Disp [78][dreg0][6B][Areg][00][disp][disp][disp]
            val mov = data.reg.getWord(op3)
            data.mem.setWord(mov, ind)
            check2(mov, 16, data)
        }
    }
  }

  private def analyze79(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    val imm = new CtxSymbol(rom.getWord(pc + 2), 16)
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

      case 0x30 =>
        //SUB.W Imm,Reg [79][3reg][imm][imm]
        val reg = data.reg.getWord(op1)
        val sub = reg - imm
        data.reg.setWord(sub, op1)
        check5(reg, imm.neg, sub, 16, data)

      case 0x40 =>
        //OR.W Imm,Reg [79][4reg][imm][imm]
        val and = data.reg.getWord(op1) | imm
        data.reg.setWord(and, op1)
        data.ccr.clearV
        check2(and, 16, data)

      case 0x50 =>
        //XOR.W Imm,Reg [79][5reg][imm][imm]
        val and = data.reg.getWord(op1) ^ imm
        data.reg.setWord(and, op1)
        data.ccr.clearV
        check2(and, 16, data)

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
    val imm = new CtxSymbol(rom.getLong(pc + 2), 32)
    data.pc.setPc(pc + 6)
    op1 & 0xF0 match {
      case 0x00 =>
        //MOV,L Imm,Reg [7A][0reg][imm][imm][imm][imm]
        data.reg.setLong(imm, op1)
        data.ccr.clearV
        check2(imm, 32, data)

      case 0x10 =>
        //ADD,L Imm,Reg [7A][1reg][imm][imm][imm][imm]
        val reg = data.reg.getLong(op1)
        val add = reg + imm
        data.reg.setLong(add, op1)
        check5(reg, imm, add, 32, data)

      case 0x20 =>
        //CMP,L Imm,Reg [7A][2reg][imm][imm][imm][imm]
        val reg = data.reg.getLong(op1)
        val cmp = reg - imm
        check5(reg, imm.neg, cmp, 32, data)

      case 0x30 =>
        //SUB,L Imm,Reg [7A][3reg][imm][imm][imm][imm]
        val reg = data.reg.getLong(op1)
        val sub = reg - imm
        data.reg.setLong(sub, op1)
        check5(reg, imm.neg, sub, 32, data)

      case 0x40 =>
        //OR.L Imm,Reg [7A][4reg][imm][imm][imm][imm]
        val and = data.reg.getLong(op1) | imm
        data.reg.setLong(and, op1)
        data.ccr.clearV
        check2(imm, 32, data)

      case 0x50 =>
        //XOR.L Imm,Reg [7A][5reg][imm][imm][imm][imm]
        val and = data.reg.getLong(op1) ^ imm
        data.reg.setLong(and, op1)
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

  private def analyze7B(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    //EEPMOV 未実装
    new ArrayBuffer[DataSet]
  }

  private def analyze7C(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc + 2) & 0xF0 match {
      case 0x60 =>
        //BTST.B Reg,IndReg [7C][indreg0][63][reg0]
        val imm = data.reg.getByte(rom.getByte(pc + 3) >> 4) & 0x07
        val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
        val mem = data.mem.getByte(ind).bitGet(imm).~
        val ccr = data.ccr.getCcr
        data.ccr.setCcr(ccr.extract(7, 3) concat mem concat ccr.extract(1, 0))
        data.pc.setPc(pc + 4)
        ArrayBuffer(data)

      case 0x70 => analyze7C7(data, pc)
    }
  }

  private def analyze7C7(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc + 2) match {
      case 0x73 =>
        //BTST.B Imm,IndReg [7C][indreg0][73][imm0]
        val imm = rom.getByte(pc + 3) >> 4
        val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
        val mem = data.mem.getByte(ind).bitGet(imm).~
        data.pc.setPc(pc + 4)
        val ccr = data.ccr.getCcr
        data.ccr.setCcr(ccr.extract(7, 3) concat mem concat ccr.extract(1, 0))
        ArrayBuffer(data)

      case 0x74 => analyze7C74(data, pc)
      case 0x75 => analyze7C75(data, pc)
      case 0x76 => analyze7C76(data, pc)
      case 0x77 => analyze7C77(data, pc)
    }
  }

  private def analyze7C74(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
    val mem = data.mem.getByte(ind).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 4)
    val and = op3 & 0x80 match {
      case 0x00 =>
        //BOR.B Imm,IndReg [7C][indreg0][74][imm0]
        ccr.extract(0, 0) | mem

      case 0x80 =>
        //BIOR.B Imm,IndReg [7C][indreg0][74][1imm0]
        ccr.extract(0, 0) | (mem.~)
    }
    data.ccr.setCcr(ccr.extract(7, 1) concat and)
    ArrayBuffer(data)
  }

  private def analyze7C75(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
    val mem = data.mem.getByte(ind).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 4)
    val and = op3 & 0x80 match {
      case 0x00 =>
        //BXOR.B Imm,IndReg [7C][indreg0][75][imm0]
        ccr.extract(0, 0) ^ mem

      case 0x80 =>
        //BIXOR.B Imm,IndReg [7C][indreg0][75][1imm0]
        ccr.extract(0, 0) ^ (mem.~)
    }
    data.ccr.setCcr(ccr.extract(7, 1) concat and)
    ArrayBuffer(data)
  }

  private def analyze7C76(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
    val mem = data.mem.getByte(ind).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 4)
    val and = op3 & 0x80 match {
      case 0x00 =>
        //BAND.B Imm,IndReg [7C][indreg0][76][imm0]
        ccr.extract(0, 0) & mem

      case 0x80 =>
        //BIAND.B Imm,IndReg [7C][indreg0][76][1imm0]
        ccr.extract(0, 0) & (mem.~)
    }
    data.ccr.setCcr(ccr.extract(7, 1) concat and)
    ArrayBuffer(data)
  }

  private def analyze7C77(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
    val mem = data.mem.getByte(ind).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 4)
    op3 & 0x80 match {
      case 0x00 =>
        //BLD.B Imm,IndReg [7C][indreg0][77][imm0]
        data.ccr.setCcr(ccr.extract(7, 1) concat mem)

      case 0x80 =>
        //BILD.B Imm,IndReg [7C][indreg0][77][1imm0]
        data.ccr.setCcr(ccr.extract(7, 1) concat (mem.~))
    }
    ArrayBuffer(data)
  }

  private def analyze7D(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc + 2) & 0xF0 match {
      case 0x60 => analyze7D6(data, pc)
      case 0x70 => analyze7D7(data, pc)
    }
  }

  private def analyze7D6(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val imm = data.reg.getByte(rom.getByte(pc + 3) >> 4) & 0x07
    val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
    rom.getByte(pc + 2) match {
      case 0x60 =>
        //BSET.B Imm,IndReg [7D][indreg0][60][reg0]
        data.pc.setPc(pc + 4)
        val bset = data.mem.getByte(ind).bitSet(imm)
        data.mem.setByte(bset, ind)
        ArrayBuffer(data)

      case 0x61 =>
        //BNOT.B Imm,IndReg [7D][indreg0][61][reg0]
        data.pc.setPc(pc + 4)
        val bnot = data.mem.getByte(ind).bitNot(imm)
        data.mem.setByte(bnot, ind)
        ArrayBuffer(data)

      case 0x62 =>
        //BCLR.B Reg,IndReg [7D][indreg0][62][reg0]
        data.pc.setPc(pc + 4)
        val bclr = data.mem.getByte(ind).bitClr(imm)
        data.mem.setByte(bclr, ind)
        ArrayBuffer(data)

      case 0x67 => analyze7C67(data, pc)
    }
  }

  private def analyze7C67(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
    val mem = data.mem.getByte(ind)
    val c = data.ccr.getCcr.extract(0, 0)
    data.pc.setPc(pc + 4)
    op3 & 0x80 match {
      case 0x00 =>
        //BST.B Imm,Indreg [7D][reg0][67][imm0]
        data.mem.setByte(mem.bitStore(c, imm), ind)

      case 0x80 =>
        //BIST.B Imm,Indreg [7D][reg0][67][1imm0]]
        data.mem.setByte(mem.bitStore(c.~, imm), ind)
    }
    ArrayBuffer(data)
  }

  private def analyze7D7(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val imm = rom.getByte(pc + 3) >> 4
    val ind = data.reg.getLong(rom.getByte(pc + 1) >> 4)
    val mem = data.mem.getByte(ind)
    data.pc.setPc(pc + 4)
    rom.getByte(pc + 2) match {
      case 0x70 =>
        //BSET.B Imm,IndReg [7D][indreg0][70][imm0]
        data.mem.setByte(mem.bitSet(imm), ind)

      case 0x71 =>
        //BNOT.B Imm,IndReg [7D][indreg0][71][imm0]
        data.mem.setByte(mem.bitNot(imm), ind)

      case 0x72 =>
        //BCLR.B Imm,Indreg [7D][indreg0][72][imm0]
        data.mem.setByte(mem.bitClr(imm), ind)
    }
    ArrayBuffer(data)
  }

  private def analyze7E(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc + 2) & 0xF0 match {
      case 0x60 =>
        //BTST.B Reg,Abs [7E][abs][63][reg0]
        val imm = data.reg.getByte(rom.getByte(pc + 3) >> 4) & 0x07
        val abs = rom.getByte(pc + 1) | 0xFFFFFF00
        val mem = data.mem.getByte(abs).bitGet(imm).~
        val ccr = data.ccr.getCcr
        data.ccr.setCcr(ccr.extract(7, 3) concat mem concat ccr.extract(1, 0))
        data.pc.setPc(pc + 4)
        ArrayBuffer(data)

      case 0x70 => analyze7E7(data, pc)
    }
  }

  private def analyze7E7(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc + 2) match {
      case 0x73 =>
        //BTST.B Imm,Abs [7E][abs][73][imm0]
        val imm = rom.getByte(pc + 3) >> 4
        val abs = rom.getByte(pc + 1) | 0xFFFFFF00
        val mem = data.mem.getByte(abs).bitGet(imm).~
        val ccr = data.ccr.getCcr
        data.ccr.setCcr(ccr.extract(7, 3) concat mem concat ccr.extract(1, 0))
        data.pc.setPc(pc + 4)
        ArrayBuffer(data)

      case 0x74 => analyze7E74(data, pc)
      case 0x75 => analyze7E75(data, pc)
      case 0x76 => analyze7E76(data, pc)
      case 0x77 => analyze7E77(data, pc)
    }
  }

  private def analyze7E74(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val mem = data.mem.getByte(rom.getByte(pc + 1)).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 4)
    val and = op3 & 0x80 match {
      case 0x00 =>
        //BOR.B Imm,Abs [7E][abs][74][imm0]
        ccr.extract(0, 0) | mem

      case 0x80 =>
        //BIOR.B Imm,Abs [7E][abs][74][1imm0]
        ccr.extract(0, 0) | (mem.~)
    }
    data.ccr.setCcr(ccr.extract(7, 1) concat and)
    ArrayBuffer(data)
  }

  private def analyze7E75(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val mem = data.mem.getByte(rom.getByte(pc + 1)).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 4)
    val and = op3 & 0x80 match {
      case 0x00 =>
        //BXOR.B Imm,Abs [7E][abs][75][imm0]
        ccr.extract(0, 0) ^ mem

      case 0x80 =>
        //BIXOR.B Imm,Abs [7E][abs][75][1imm0]
        ccr.extract(0, 0) ^ (mem.~)
    }
    data.ccr.setCcr(ccr.extract(7, 1) concat and)
    ArrayBuffer(data)
  }

  private def analyze7E76(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val mem = data.mem.getByte(rom.getByte(pc + 1)).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 4)
    val and = op3 & 0x80 match {
      case 0x00 =>
        //BAND.B Imm,Abs [7E][abs][76][imm0]
        ccr.extract(0, 0) & mem

      case 0x80 =>
        //BIAND.B Imm,Abs [7E][abs][76][1imm0]
        ccr.extract(0, 0) & (mem.~)
    }
    data.ccr.setCcr(ccr.extract(7, 1) concat and)
    ArrayBuffer(data)
  }

  private def analyze7E77(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    val imm = (op3 >> 4) & 0x07
    val mem = data.mem.getByte(rom.getByte(pc + 1)).bitGet(imm)
    val ccr = data.ccr.getCcr
    data.pc.setPc(pc + 4)
    op3 & 0x80 match {
      case 0x00 =>
        //BLD.B Imm,Abs [7E][abs][77][imm0]
        data.ccr.setCcr(ccr.extract(7, 1) concat mem)

      case 0x80 =>
        //BILD.B Imm,Abs [7E][abs][77][1imm0]
        data.ccr.setCcr(ccr.extract(7, 1) concat (mem.~))
    }
    ArrayBuffer(data)
  }

  private def analyze7F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    data.pc.setPc(pc + 4)
    rom.getByte(pc + 2) & 0xF0 match {
      case 0x60 => analyze7F6(data, pc)
      case 0x70 => analyze7F7(data, pc)
    }
  }

  private def analyze7F6(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val abs = rom.getByte(pc + 1)
    val imm = data.reg.getByte(rom.getByte(pc + 3) >> 4) & 0x07
    val mem = data.mem.getByte(abs)
    rom.getByte(pc + 2) match {
      case 0x60 =>
        //BSET.B Reg,Abs [7F][abs][60][reg0]
        data.mem.setByte(mem.bitSet(imm), abs)
        data.pc.setPc(pc + 4)
        ArrayBuffer(data)

      case 0x61 =>
        //BNOT.B Reg,Abs [7F][abs][61][reg0]
        data.mem.setByte(mem.bitNot(imm), abs)
        data.pc.setPc(pc + 4)
        ArrayBuffer(data)

      case 0x62 =>
        //BCLR.B Reg,Abs [7F][abs][62][reg0]
        data.mem.setByte(mem.bitClr(imm), abs)
        data.pc.setPc(pc + 4)
        ArrayBuffer(data)

      case 0x67 => analyze7F67(data, pc)
    }
  }

  private def analyze7F67(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val abs = rom.getByte(pc + 1) | 0xFFFFFF00
    val mem = data.mem.getByte(abs)
    val imm = (rom.getByte(pc + 3) >> 4) & 0x07
    val c = data.ccr.getCcr.extract(0, 0)
    data.pc.setPc(pc + 4)
    rom.getByte(pc + 3) match {
      case 0x00 =>
        //BST.B Imm,Abs [7F][abs][67][imm0]
        data.mem.setByte(mem.bitStore(c, imm), abs)

      case 0x80 =>
        //BIST.B Imm,Abs [7F][abs][67][1imm0]
        data.mem.setByte(mem.bitStore(c.~, imm), abs)
    }
    ArrayBuffer(data)
  }

  private def analyze7F7(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val abs = rom.getByte(pc + 1)
    val op3 = rom.getByte(pc + 3)
    val imm = op3 >> 4
    val mem = data.mem.getByte(abs)
    data.pc.setPc(pc + 4)
    rom.getByte(pc + 2) match {
      case 0x70 =>
        //BSET.B Imm,Abs [7F][abs][70][reg0]
        data.mem.setByte(mem.bitSet(imm), abs)

      case 0x71 =>
        //BNOT.B Imm,Abs [7F][abs][71][reg0]
        data.mem.setByte(mem.bitNot(imm), abs)

      case 0x72 =>
        //BCLR.B Imm,Abs [7F][abs][72][reg0]
        data.mem.setByte(mem.bitClr(imm), abs)
    }
    ArrayBuffer(data)
  }

  //Z,Nフラグまとめてチェック
  def check2(num: CtxSymbol, size: Int, base: DataSet): ArrayBuffer[DataSet] = {
    val buf = checkZ(num, base, size)
    checkN(num, buf, size)
  }

  def check2(num: CtxSymbol, size: Int, base: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    base.foreach { b => ans ++= check2(num, size, b)}
    ans
  }

  //V,Z,Nフラグまとめてチェック
  def check3(num1: CtxSymbol, num2: CtxSymbol, result: CtxSymbol, size: Int, base: DataSet): ArrayBuffer[DataSet] = {
    val buf = checkV(num1, num2, result, base, size)
    check2(result, size, buf)
  }

  def check3(num1: CtxSymbol, num2: CtxSymbol, result: CtxSymbol, size: Int, base: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    base.foreach { b => ans ++= check3(num1, num2, result, size, b)}
    ans
  }

  //5つまとめてチェック
  def check5(num1: CtxSymbol, num2: CtxSymbol, result: CtxSymbol, size: Int, base: DataSet): ArrayBuffer[DataSet] = {
    var buf = checkC(num1, num2, result, base, size)
    buf = check3(num1, num2, result, size, buf)
    checkH(num1, num2, result, buf, size)
  }

  private def checkC(num1: CtxSymbol, num2: CtxSymbol, res: CtxSymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b => checkC(num1, num2, res, b, size)}
    ans
  }

  private def checkC(num1: CtxSymbol, num2: CtxSymbol, res: CtxSymbol, data: DataSet, size: Int): ArrayBuffer[DataSet] = {
    if ((data.ccr.check & 0x01) == 0x01) {
      //条件分岐にCフラグを使うものがなければ，分ける必要がない
      val s = size - 1
      val bool1 = num1.extract(s, s).equal(1) && num2.extract(s, s).equal(1)
      val bool2 = (num1.extract(s, s).equal(1) || num2.extract(s, s).equal(1)) && res.extract(s, s).equal(0)
      val clone = twoPathClone(bool1 || bool2, data)
      clone._1.ccr.setC
      clone._2.ccr.clearC
      tapleToArray(clone)
    } else ArrayBuffer(data)
  }

  //  private def checkV(num1: CtxSymbol, num2: CtxSymbol, res: CtxSymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
  //    val ans = new ArrayBuffer[DataSet]
  //    buf.foreach { b => ans ++= checkV(num1, num2, res, b, size)}
  //    ans
  //  }

  private def checkV(num1: CtxSymbol, num2: CtxSymbol, res: CtxSymbol, data: DataSet, size: Int): ArrayBuffer[DataSet] = {
    if ((data.ccr.check & 0x02) == 0x02) {
      //条件分岐にVフラグを使うものがなければ，分ける必要がない
      val bool1 = (num1 >= 0) && (num2 >= 0) && (res < 0)
      val bool2 = (num1 < 0) && (num2 < 0) && (res >= 0)
      val clone = twoPathClone(bool1 || bool2, data)
      clone._1.ccr.setV
      clone._2.ccr.clearV
      tapleToArray(clone)
    } else ArrayBuffer(data)
  }

  private def checkZ(num: CtxSymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b => ans ++= checkZ(num, b, size)}
    ans
  }

  private def checkZ(num: CtxSymbol, data: DataSet, size: Int): ArrayBuffer[DataSet] = {
    if ((data.ccr.check & 0x04) == 0x04) {
      //条件分岐にZフラグを使うものがなければ，分ける必要がない
      val clone = twoPathClone(num.equal(0), data)
      clone._1.ccr.setZ //data = 0
      clone._2.ccr.clearZ
      tapleToArray(clone)
    } else ArrayBuffer(data)
  }

  private def checkN(num: CtxSymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b => ans ++= checkN(num, b, size)}
    ans
  }

  private def checkN(num: CtxSymbol, data: DataSet, size: Int): ArrayBuffer[DataSet] = {
    if ((data.ccr.check & 0x08) == 0x08) {
      //条件分岐にNフラグを使うものがなければ，分ける必要がない
      val clone = twoPathClone(num < 0, data)
      clone._1.ccr.setN //data < 0
      clone._2.ccr.clearN
      tapleToArray(clone)
    } else ArrayBuffer(data)
  }

  private def checkH(data1: CtxSymbol, data2: CtxSymbol, res: CtxSymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b => ans ++= checkH(data1, data2, res, b, size)}
    ans
  }

  private def checkH(num1: CtxSymbol, num2: CtxSymbol, res: CtxSymbol, data: DataSet, size: Int): ArrayBuffer[DataSet] = {
    if ((data.ccr.check & 0x20) == 0x20) {
      //条件分岐にHフラグを使うものがなければ，分ける必要がない
      val s = size - 5
      val bool1 = num1.extract(s, s).equal(1) && num2.extract(s, s).equal(1)
      val bool2 = (num1.extract(s, s).equal(1) || num2.extract(s, s).equal(1)) && res.extract(s, s).equal(0)
      val clone = twoPathClone(bool1 || bool2, data)
      clone._1.ccr.setC
      clone._2.ccr.clearC
      tapleToArray(clone)
    } else ArrayBuffer(data)
  }

  //twoPathCloneで作ったタプルを可変長Arrayに変換する
  private def tapleToArray(data: (DataSet, DataSet)): ArrayBuffer[DataSet] = ArrayBuffer(data._1, data._2)

  //条件によって2分岐するときに使う
  //clone1が条件true
  //clone2が条件false
  private def twoPathClone(path: CtxSymbol, data: DataSet): (DataSet, DataSet) = {
    val clone1 = data.clone
    clone1.path.set(path.symbol)
    val clone2 = data.clone
    clone2.path.set(path.not.symbol)
    clone1 -> clone2
  }
}
