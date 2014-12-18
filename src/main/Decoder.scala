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

  def analyze(data: DataSet): ArrayBuffer[DataSet] = {
    analyze(data.clone, data.pc.pc)
  }

  def check2(data: MySymbol, size: Int, base: DataSet): ArrayBuffer[DataSet] = check2(data, size, ArrayBuffer(base))

  def check2(data: MySymbol, size: Int, base: ArrayBuffer[DataSet]): ArrayBuffer[DataSet] = {
    val buf = checkZ(data, base)
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

  private def analyze(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    op0 & 0xF0 match {
      case 0x00 => analyze0(data, pc)

      case 0x10 => analyze1(data, pc)

      case 0x20 =>
        //MOV.B Abs,Reg [2reg][abs]
        val mov = data.mem.getByte(rom.getByte(pc + 1) | 0xFFFFFF00)
        data.reg.setByte(mov, op0)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(mov, 8, data)

      case 0x30 =>
        //MOV.B Reg,Abs [3reg][abs]
        val mov = data.reg.getByte(op0)
        data.mem.setByte(mov, rom.getByte(pc + 1) | 0xFFFFFF00)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(mov, 8, data)

      case 0x40 => analyze4(data, pc)

      case 0x50 => analyze5(data, pc)

      case 0x60 => analyze6(data, pc)

      case 0x70 => analyze7(data, pc)

      case 0x80 =>
        //ADD.B Imm,Reg [8Reg][Imm]
        val imm = new IntSymbol(rom.getByte(pc + 1))
        val reg = data.reg.getByte(op0)
        val add = reg + imm
        data.reg.setByte(add, op0)
        data.pc.setPc(pc + 2)
        check5(reg, imm, add, 8, data)

      case 0x90 =>
        //ADDX.B Imm,Reg [9reg][imm]
        val reg = data.reg.getByte(op0)
        data.pc.setPc(pc + 2)
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val imm = new IntSymbol(rom.getByte(pc + 1)) + (if ((ccr & 0x01).eq(0x01)) 1 else 0)
            val add = reg + imm
            data.reg.setByte(add, op0)
            check5(reg, imm, add, 8, data)
          case ccr: CtxSymbol =>
            //キャリ(c)がfalseの時
            val imm = new IntSymbol(rom.getByte(pc + 1))
            val bool = (ccr & 0x01).eq(0x01)
            val d1 = data.clone
            val add = reg + imm
            d1.reg.setByte(add, op0)
            d1.path.set(bool.not.symbol)
            val buf = new ArrayBuffer[DataSet]
            buf ++= check5(reg, imm, add, 8, d1)
            //キャリがtrueの時
            val d2 = data.clone
            d2.reg.setByte(add + 1, op0)
            d2.path.set(bool.symbol)
            buf ++= check5(reg, imm + 1, add + 1, 8, d1)
            buf
        }

      case 0xA0 =>
        //CMP.B Imm,Reg [Areg][imm]
        val imm = new IntSymbol(rom.getByte(pc + 1))
        val reg = data.reg.getByte(op0)
        val cmp = reg - imm
        data.pc.setPc(pc + 2)
        check5(reg, imm.neg, cmp, 8, data)

      case 0xB0 =>
        //SUBX.B Imm,Reg [Breg][imm]
        val reg = data.reg.getByte(op0)
        data.pc.setPc(pc + 2)
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val imm = new IntSymbol(rom.getByte(pc + 1)) + (if ((ccr & 0x01).eq(0x01)) 1 else 0)
            val add = reg - imm
            data.reg.setByte(add, op0)
            check5(reg, imm.neg, add, 8, data)
          case ccr: CtxSymbol =>
            //キャリ(c)がfalseの時
            val imm = new IntSymbol(rom.getByte(pc + 1))
            val bool = (ccr & 0x01).eq(0x01)
            val d1 = data.clone
            val add = reg - imm
            d1.reg.setByte(add, op0)
            d1.path.set(bool.not.symbol)
            val buf = new ArrayBuffer[DataSet]
            buf ++= check5(reg, imm.neg, add, 8, d1)
            //キャリがtrueの時
            val d2 = data.clone
            d2.reg.setByte(add - 1, op0)
            d2.path.set(bool.symbol)
            buf ++= check5(reg, (imm - 1).neg, add - 1, 8, d1)
            buf
        }

      case 0xC0 =>
        //OR.B Imm,Reg [Ereg][Imm]
        val or = data.reg.getByte(op0) | new IntSymbol(rom.getByte(pc + 1))
        data.reg.setByte(or, op0)
        data.ccr.clearV
        data.pc.setPc(pc + 2)
        check2(or, 8, data)

      case 0xE0 =>
        //XOR.B Imm,Reg [Ereg][Imm]
        val xor = data.reg.getByte(op0) ^ new IntSymbol(rom.getByte(pc + 1))
        data.reg.setByte(xor, op0)
        data.ccr.clearV
        data.pc.setPc(pc + 2)
        check2(xor, 8, data)

      case 0xE0 =>
        //AND.B Imm,Reg [Ereg][Imm]
        val and = data.reg.getByte(op0) & new IntSymbol(rom.getByte(pc + 1))
        data.reg.setByte(and, op0)
        data.ccr.clearV
        data.pc.setPc(pc + 2)
        check2(and, 8, data)

      case 0xF0 =>
        //MOV.B Imm,Reg [Freg][Imm]
        val imm = new IntSymbol(rom.getByte(pc + 1))
        data.reg.setByte(imm, op0)
        data.ccr.clearV
        data.pc.setPc(pc + 2)
        check2(imm, 8, data)
    }
  }

  private def analyze0(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op0 = rom.getByte(pc)
    op0 match {
      case 0x00 =>
        //NOP [00][00]
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x01 => analyze01(data, pc)

      case 0x02 =>
        //STC.B CCR,Reg [02][0reg]
        data.reg.setByte(data.ccr.ccr, rom.getByte(pc + 1))
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x03 =>
        //LDC.B Reg,CCR [03][0reg]
        data.ccr.ccr = data.reg.getByte(rom.getByte(pc + 1))
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x04 =>
        //ORC.B Imm,Ccr [04][imm]
        data.ccr.ccr = data.ccr.ccr | rom.getByte(pc + 1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x05 =>
        //ANDC.B Imm,Ccr [06][imm]
        data.ccr.ccr = data.ccr.ccr ^ rom.getByte(pc + 1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x06 =>
        //ANDC.B Imm,Ccr [06][imm]
        data.ccr.ccr = data.ccr.ccr & rom.getByte(pc + 1)
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x07 =>
        //LDC.B Reg,CCR [03][imm]
        data.ccr.ccr = new IntSymbol(rom.getByte(pc + 1))
        data.pc.setPc(pc + 2)
        ArrayBuffer(data)

      case 0x08 =>
        //ADD.B RegA,RegB [08][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val add = regB + regA
        data.reg.setByte(add, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA, add, 8, data)

      case 0x09 =>
        //ADD.W RegA,RegB [09][regAregB]
        val op1 = rom.getByte(pc + 1)
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
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4)
        data.reg.setWord(regA, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(regA, 8, data)

      case 0x0D =>
        //MOV.W RegA,RegB [0D][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4)
        data.reg.setWord(regA, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        check2(regA, 16, data)

      case 0x0E =>
        //ADDX.B RegA,RegB [0E][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regB = data.reg.getByte(op1)
        data.pc.setPc(pc + 2)
        data.ccr.ccr match {
          case ccr: IntSymbol =>
            val regA = data.reg.getByte(op1 >> 4) + (if ((ccr & 0x01).eq(0x01)) 1 else 0)
            val add = regB + regA
            data.reg.setByte(add, op1)
            check5(regB, regA, add, 8, data)
          case ccr: CtxSymbol =>
            //キャリ(c)がfalseの時
            val regA = data.reg.getByte(op1 >> 4)
            val bool = (ccr & 0x01).eq(0x01)
            val d1 = data.clone
            val add = regA + regB
            d1.reg.setByte(add, op1)
            d1.path.set(bool.not.symbol)
            val buf = new ArrayBuffer[DataSet]
            buf ++= check5(regB, regA, add, 8, d1)
            //キャリがtrueの時
            val d2 = data.clone
            d2.reg.setByte(add + 1, op1)
            d2.path.set(bool.symbol)
            buf ++= check5(regB, regA + 1, add + 1, 8, d1)
        }

      case 0x0F => analyze0F(data, pc)
    }
  }

  private def analyze01(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    rom.getByte(pc + 1) & 0xF0 match {
      case 0x00 => analyze0100(data, pc)
      case 0x04 => analyze0140(data, pc)
      case 0xF0 => analyze01F06(data, pc)
    }
  }

  private def analyze0100(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op3 = rom.getByte(pc + 3)
    data.ccr.clearV
    val buf = new ArrayBuffer[DataSet]
    rom.getByte(pc + 2) match {
      case 0x69 =>
        data.pc.setPc(pc + 4)
        op3 & 0x80 match {
          case 0x00 =>
            //MOV.L Indreg,Reg [01][00][69][indreg reg]
            data.reg.getLong(op3 >> 4) match {
              case ind: IntSymbol =>
                val mov = data.mem.getLong(ind.symbol)
                data.reg.setLong(mov, op3)
                buf ++= check2(mov, 32, data)
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  clone.path.set(ind.extract(15, 0).eq(p).symbol)
                  val mov = clone.mem.getLong(p)
                  clone.reg.setLong(mov, op3)
                  buf ++= check2(mov, 32, clone)
                }
            }

          case 0x80 =>
            //MOV.L Reg,IndReg [01][00][69][1indreg reg]
            val mov = data.reg.getLong(op3)
            data.reg.getLong(op3 >> 4) match {
              case ind: IntSymbol =>
                data.mem.setLong(mov, ind.symbol)
                buf ++= check2(mov, 32, data)
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  clone.path.set(ind.extract(15, 0).eq(p).symbol)
                  clone.mem.setLong(mov, p)
                  buf ++= check2(mov, 32, clone)
                }
            }
        }

      case 0x6B =>
        op3 & 0x80 match {
          case 0x00 =>
            //MOV.L @Reg+,Reg [01][00][6B][@regreg]
            data.reg.getLong(op3 >> 4) match {
              case ind: IntSymbol =>
                val mov = data.mem.getLong(ind.symbol)
                data.reg.setLong(mov, op3)
                data.reg.setLong(ind + 4, op3 >> 4)
                buf ++= check2(mov, 32, data)
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  clone.path.set(ind.extract(15, 0).eq(p).symbol)
                  val mov = clone.mem.getLong(p)
                  clone.reg.setLong(mov, op3)
                  clone.reg.setLong(ind + 4, op3)
                  buf ++= check2(mov, 32, clone)
                }
            }

          case 0x80 =>
            //MOV.L Reg,@-Reg [01][00][6B][1pregreg]
            val mov = data.reg.getLong(op3)
            data.reg.getLong(op3 >> 4) - 4 match {
              case ind: IntSymbol =>
                data.mem.setLong(mov, ind.symbol)
                data.reg.setLong(ind, op3 >> 4)
                check2(mov, 32, data)
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  clone.path.set(ind.extract(15, 0).eq(p).symbol)
                  clone.mem.setLong(mov, p)
                  clone.reg.setLong(ind, op3 >> 4)
                  buf ++= check2(mov, 32, clone)
                }
            }
        }

      case 0x6D =>
        op3 & 0xF0 match {
          case 0x00 =>
            //MOV.L Abs,Reg [01][00][6D][0reg][abs][abs]

          case 0x20 =>

          case 0x80 =>
          //MOV.L Reg,Abs [01][00][6D][8reg][abs][abs]

          case 0xA0 =>
        }

      case 0x6F =>
        val disp = rom.getWord(pc + 2)
        data.pc.setPc(pc + 6)
        op3 & 0x80 match {
          case 0x00 =>
            //MOV.L DIsp16,Reg [01][00][6F][dregreg][disp][disp]
            data.reg.getLong(op3 >> 4) match {
              case ind: IntSymbol =>
                data.reg.getLong(ind.symbol) + disp match {
                  case d: IntSymbol =>
                    val mov = data.mem.getLong(d.symbol)
                    data.reg.setLong(mov, op3)
                    buf ++= check2(mov, 32, data)
                  case d: CtxSymbol =>
                    extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
                      val clone = data.clone
                      clone.path.set(d.extract(15, 0).eq(p).symbol)
                      val mov = clone.mem.getLong(p)
                      clone.reg.setLong(mov, op3)
                      buf ++= check2(mov, 32, clone)
                    }
                }
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  clone.path.set(ind.extract(15, 0).eq(p).symbol)
                  val mov = clone.mem.getLong(p + disp)
                  clone.reg.setLong(mov, op3)
                  buf ++= check2(mov, 32, clone)
                }
            }

          case 0x80 =>
            val mov = data.reg.getLong(op3)
            //MOV.L Reg,Disp16 [01][00][6F][1dregreg][disp][disp]
            data.reg.getLong(op3 >> 4) match {
              case ind: IntSymbol =>
                data.reg.getLong(ind.symbol) + disp match {
                  case d: IntSymbol =>
                    data.mem.setLong(mov, d.symbol)
                    buf ++= check2(mov, 32, data)
                  case d: CtxSymbol =>
                    extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
                      val clone = data.clone
                      clone.path.set(d.extract(15, 0).eq(p).symbol)
                      clone.mem.setLong(mov, p)
                      buf ++= check2(mov, 32, clone)
                    }
                }
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  clone.path.set(ind.extract(15, 0).eq(p).symbol)
                  clone.mem.setLong(mov, p + disp)
                  buf ++= check2(mov, 32, clone)
                }
            }
        }
        buf

      case 0x78 =>
        val disp = rom.getLong(pc + 6)
        val op5 = rom.getByte(pc + 5)
        data.pc.setPc(pc + 10)
        op3 & 0x80 match {
          case 0x00 =>
            //MOV.L Disp32,Reg [01][00][78][dreg0][6B][2reg][00][disp][disp][disp]
            data.reg.getLong(op3 >> 4) match {
              case ind: IntSymbol =>
                data.reg.getLong(ind.symbol) + disp match {
                  case d: IntSymbol =>
                    val mov = data.mem.getLong(d.symbol)
                    data.reg.setLong(mov, op5)
                    buf ++= check2(mov, 32, data)
                  case d: CtxSymbol =>
                    extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
                      val clone = data.clone
                      clone.path.set(d.extract(15, 0).eq(p).symbol)
                      val mov = clone.mem.getLong(p)
                      clone.reg.setLong(mov, op5)
                      buf ++= check2(mov, 32, clone)
                    }
                }
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  clone.path.set(ind.extract(15, 0).eq(p).symbol)
                  val mov = clone.mem.getLong(p + disp)
                  clone.reg.setLong(mov, op5)
                  buf ++= check2(mov, 32, clone)
                }
            }

          case 0x80 =>
            val mov = data.reg.getLong(op5)
            //MOV.L Reg,Disp32 [01][00][78][1dreg0][6B][Areg][00][disp][disp][disp]
            data.reg.getLong(op3 >> 4) match {
              case ind: IntSymbol =>
                data.reg.getLong(ind.symbol) + disp match {
                  case d: IntSymbol =>
                    data.mem.setLong(mov, d.symbol)
                    buf ++= check2(mov, 32, data)
                  case d: CtxSymbol =>
                    extract(0 to 0xFFFF, d.extract(15, 0)).foreach { p =>
                      val clone = data.clone
                      clone.path.set(d.extract(15, 0).eq(p).symbol)
                      clone.mem.setLong(mov, p)
                      buf ++= check2(mov, 32, clone)
                    }
                }
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  clone.path.set(ind.extract(15, 0).eq(p).symbol)
                  clone.mem.setLong(mov, p + disp)
                  buf ++= check2(mov, 32, clone)
                }
            }
        }
    }
    buf
  }

  private def analyze0140(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {

  }

  private def analyze01F06(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 3)
    val regA = data.reg.getLong(op1 >> 4)
    val regB = data.reg.getLong(op1)
    data.pc.setPc(pc + 4)
    data.ccr.clearV
    val logic =
      rom.getByte(pc + 2) match {
        case 0x64 =>
          //OR.L RegA,RegB [01][F0][64][regAregB]
          regB | regA

        case 0x64 =>
          //XOR.L RegA,RegB [01][F0][65][regAregB]
          regB ^ regA

        case 0x66 =>
          //AND.L RegA,RegB [01][F0][66][regAregB]
          regB & regA
      }
    check2(logic, 32, data)
  }

  private def analyze0A(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    op1 & 0xF0 match {
      case 0x00 =>
        //INC.B Reg [0A][0reg]
        val reg = data.reg.getByte(op1)
        val inc = reg + 1
        data.reg.setByte(inc, op1)
        data.pc.setPc(pc + 2)
        check3(reg, new IntSymbol(1), inc, 8, data)
      case _ =>
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
    var buf = new ArrayBuffer[DataSet]
    val op1 = rom.getByte(pc + 1)
    op1 & 0xF0 match {
      case 0x00 =>
        //ADDS.L #1,Reg [0B][0reg]
        val add = data.reg.getLong(op1) + 1
        data.reg.setLong(add, op1)
        data.pc.setPc(pc + 2)
        buf += data

      case 0x50 =>
        //INC.W #1,Reg [0B][5reg]
        val reg = data.reg.getWord(op1) + 1
        val inc = reg + 1
        data.reg.setWord(inc, op1)
        data.pc.setPc(pc + 2)
        buf = checkV(reg, new IntSymbol(1), inc, ArrayBuffer(data), 16)
        buf = checkZ(inc, buf)
        buf = checkN(inc, buf, 16)

      case 0x80 =>
        //ADDS.L #2,Reg [0B][8reg]
        val add = data.reg.getLong(op1) + 2
        data.reg.setLong(add, op1)
        data.pc.setPc(pc + 2)
        buf += data

      case 0x90 =>
        //ADDS.L #4,Reg [0B][9reg]
        val add = data.reg.getLong(op1) + 4
        data.reg.setLong(add, op1)
        data.pc.setPc(pc + 2)
        buf += data
    }
    buf
  }

  private def analyze0F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val op1 = rom.getByte(pc + 1)
    op1 & 0xF0 match {
      //case 0x00 =>
      //DAA.B Reg [0F][0reg]

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
    val op0 = rom.getByte(pc)
    op0 match {
      case 0x16 =>
        //AND.B RegA,RegB [16][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val and = regB & regA
        data.reg.setByte(and, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        val buf = checkZ(and, ArrayBuffer(data))
        checkN(and, buf, 8)

      case 0x17 => analyze17(data, pc)

      case 0x18 =>
        //SUB.B RegA,RegB [18][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val sub = regB - regA
        data.reg.setByte(sub, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA.neg, sub, 8, data)

      case 0x19 =>
        //SUB.W RegA,RegB [19][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4)
        val regB = data.reg.getByte(op1)
        val sub = regB - regA
        data.reg.setWord(sub, op1)
        data.pc.setPc(pc + 2)
        check5(regB, regA.neg, sub, 16, data)

      case 0x1D =>
        //CMP.W RegA,RegB [1D][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val sub = regB - regA
        data.pc.setPc(pc + 2)
        check5(regB, regA.neg, sub, 16, data)
    }
  }

  private def analyze17(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    val op1 = rom.getByte(pc + 1)
    op1 & 0xF0 match {
      case 0x00 =>
        //NOT.B Reg [17][0reg]
        val not = data.reg.getByte(op1).~
        data.reg.setByte(not, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        buf = checkZ(not, ArrayBuffer(data))
        buf = checkN(not, buf, 8)

      case 0x50 =>
        //EXTU.W Reg [17][5reg]
        val extu = data.reg.getWord(op1) & 0xFF
        data.reg.setWord(extu, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearN
        data.ccr.clearV
        buf += data
        buf = checkZ(extu, ArrayBuffer(data))
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
        data.reg.setLong(sp + 2, 7)
        data.mem.getWord(sp.symbol) match {
          case reg: IntSymbol =>
            data.pc.setPc(reg.symbol)
            buf += data
          case reg: CtxSymbol =>
            extract(0 to 0xFFFF, reg).foreach { p =>
              val clone = data.clone
              clone.pc.setPc(p)
              clone.path.set(reg.eq(p).symbol)
              buf += clone
            }
        }

      case 0x56 =>
        //RTE [56][70]
        val sp = data.reg.getLong(7).asInstanceOf[IntSymbol]
        data.mem.getLong(sp.symbol) match {
          case reg: IntSymbol =>
            data.pc.setPc((reg & 0xFFFF).symbol)
            data.ccr.ccr = (reg >> 24) & 0xFF
            buf += data
          case reg: CtxSymbol =>
            val setpc = reg.extract(15, 0)
            data.ccr.ccr = reg.extract(31, 24)
            extract(0 to 0xFFFF, setpc).foreach { p =>
              val clone = data.clone
              clone.pc.setPc(p)
              clone.path.set(setpc.eq(p).symbol)
              buf += clone
            }
        }

      case 0x59 =>
        //JMP IndReg [59][reg0]
        val op1 = rom.getByte(pc + 1)
        data.reg.getLong(op1 >> 4) match {
          case abs: IntSymbol =>
            data.pc.setPc(abs.symbol)
            buf += data
          case abs: CtxSymbol =>
            extract(0 to 0xFFFF, abs.extract(15, 0)).foreach { p =>
              val clone = data.clone
              data.pc.setPc(p)
              clone.path.set(abs.extract(15, 0).eq(p).symbol)
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
      case 0x71 =>
        //BNOT.B RegA,RegB [61][regAregB]
        val op1 = rom.getByte(pc + 1)
        val imm = data.reg.getByte(op1 >> 4)
        data.pc.setPc(pc + 2)
        data.reg.getByte(op1) >> imm match {
          case reg: IntSymbol =>
            val bnot = if ((reg & 0x01).eq(0x01)) reg.bitclr(imm) else reg.bitset(imm)
            data.reg.setByte(bnot, op1)
            buf += data
          case reg: CtxSymbol =>
            val bool = (reg & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8)).asInstanceOf[CtxSymbol]
            val d1 = data.clone
            d1.reg.setByte(reg.bitclr(imm), op1)
            d1.path.set(bool.symbol)
            buf += d1
            val d2 = data.clone
            d2.reg.setByte(reg.bitset(imm), op1)
            d2.path.set(bool.not.symbol)
            buf += d2
        }

      case 0x62 =>
        //BCLR.B RegA,RegB [62][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getByte(op1 >> 4) & 0x07
        val regB = data.reg.getByte(op1)
        val bclr = regB.bitclr(regA)
        data.reg.setByte(bclr, op1)
        data.pc.setPc(pc + 2)
        buf += data

      case 0x66 =>
        //AND.W RegA,RegB [66][regAregB]
        val op1 = rom.getByte(pc + 1)
        val regA = data.reg.getWord(op1 >> 4)
        val regB = data.reg.getWord(op1)
        val and = regB & regA
        data.reg.setWord(and, op1)
        data.pc.setPc(pc + 2)
        data.ccr.clearV
        buf = checkZ(and, ArrayBuffer(data))
        buf = checkN(and, buf, 16)

      case 0x67 =>
        val op1 = rom.getByte(pc + 1)
        val imm = (op1 >> 4) & 0x07
        val reg = data.reg.getByte(op1)
        val ccr = data.ccr.ccr
        op1 & 0x80 match {
          case 0x00 =>
            //BST.B Imm,Reg [67][imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bst = if ((c & 0x01).eq(0x01)) r | (1 << imm) else r & (~(1 << imm))
                data.reg.setByte(bst, op1)
                buf += data
              case _ =>
                val bool = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8)).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.reg.setByte(reg | (1 << imm), op1)
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.reg.setByte(reg & (~(1 << imm)), op1)
                d2.path.set(bool.not.symbol)
                buf += d2
            }
          case 0x80 =>
            //BIST.B Imm,Reg [67][1imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bst = if ((c & 0x01).eq(0x00)) r | (1 << imm) else r & (~(1 << imm))
                data.reg.setByte(bst, op1)
                buf += data
              case _ =>
                val bool = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8)).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.reg.setByte(reg | (1 << imm), op1)
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.reg.setByte(reg & (~(1 << imm)), op1)
                d2.path.set(bool.not.symbol)
                buf += d2
            }
        }

      case 0x68 =>
        val op1 = rom.getByte(pc + 1)
        op1 & 0x80 match {
          case 0x00 =>
            //MOV.B IndReg,Reg [68][0indreg reg]
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            data.reg.getLong(op1 >> 4) match {
              case abs: IntSymbol =>
                val mov = data.mem.getByte(abs.symbol)
                data.reg.setByte(mov, op1)
                buf = checkZ(mov, ArrayBuffer(data))
                buf = checkN(mov, buf, 8)
              case abs: CtxSymbol =>
                extract(0 to 0xFFFF, abs.extract(15, 0)).foreach { p =>
                  val clone = data.clone
                  val mov = clone.mem.getByte(p)
                  clone.reg.setByte(mov, op1)
                  clone.path.set(abs.extract(15, 0).eq(p).symbol)
                  var buff = ArrayBuffer(clone)
                  buff = checkZ(mov, buff)
                  buf ++= checkN(mov, buff, 8)
                }
            }

          case 0x80 =>
            //MOV,B Reg,IndReg [68][1indreg reg]
            val mov = data.reg.getByte(op1)
            data.pc.setPc(pc + 2)
            data.ccr.clearV
            data.reg.getLong(op1 >> 4) match {
              case abs: IntSymbol =>
                data.mem.setByte(mov, abs.symbol)
                buf = checkZ(mov, ArrayBuffer(data))
                buf = checkN(mov, buf, 8)
              case abs: CtxSymbol =>
                extract(0 to 0xFFFF, abs.extract(15, 0).symbol).foreach { p =>
                  val clone = data.clone
                  clone.mem.setByte(mov, p)
                  clone.path.set(abs.extract(15, 0).eq(p).symbol)
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
        data.pc.setPc(pc + 4)
        data.ccr.clearV
        data.reg.getLong(op1 >> 4) match {
          case dreg: IntSymbol =>
            val add = dreg + disp
            val mov = data.mem.getByte(add.symbol)
            data.reg.setByte(mov, op1)
            buf = checkZ(mov, ArrayBuffer(data))
            buf = checkN(mov, buf, 8)
          case dreg: CtxSymbol =>
            val add = dreg.extract(15, 0) + disp
            extract(0 to 0xFFFF, add).foreach { a =>
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
      case 0x71 =>
        //BNOT.B Imm,Reg [71][immreg]
        val op1 = rom.getByte(pc + 1)
        val imm = op1 >> 4
        data.pc.setPc(pc + 2)
        data.reg.getByte(op1) >> imm match {
          case reg: IntSymbol =>
            val bnot = if ((reg & 0x01).eq(0x01)) reg.bitclr(imm) else reg.bitset(imm)
            data.reg.setByte(bnot, op1)
            buf += data
          case reg: CtxSymbol =>
            val bool = (reg & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8)).asInstanceOf[CtxSymbol]
            val d1 = data.clone
            d1.reg.setByte(reg.bitclr(imm), op1)
            d1.path.set(bool.symbol)
            buf += d1
            val d2 = data.clone
            d2.reg.setByte(reg.bitset(imm), op1)
            d2.path.set(bool.not.symbol)
            buf += d2
        }

      case 0x72 =>
        //BCLR.B Imm,Reg [72][immreg]
        val op1 = rom.getByte(pc + 1)
        val imm = op1 >> 4
        val reg = data.reg.getByte(op1)
        val bclr = reg.bitclr(imm)
        data.reg.setByte(bclr, op1)
        data.pc.setPc(pc + 2)
        buf += data

      case 0x74 =>
        val op1 = rom.getByte(pc + 1)
        val imm = (op1 >> 4) & 0x07
        val reg = data.reg.getByte(op1) >> imm
        val ccr = data.ccr.ccr
        op1 & 0x80 match {
          case 0x00 =>
            //BOR.B Imm,Reg [74][imm reg]
            (reg, ccr) match {
              case (r: IntSymbol, c: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x01)
                if (bool1 | bool2) data.ccr.setC else data.ccr.clearC
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (reg & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool = (bool1 | bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }
          case 0x80 =>
            //BIOR.B Imm,Reg [74][1imm reg]
            (reg, ccr) match {
              case (r: IntSymbol, c: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x00)
                if (bool1 | bool2) data.ccr.setC else data.ccr.clearC
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (reg & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                val bool = (bool1 | bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }
        }

      case 0x75 =>
        val op1 = rom.getByte(pc + 1)
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
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (reg & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool = (bool1 ^^ bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }

          case 0x80 =>
            //BIXOR.B Imm,Reg [75][1imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x00)
                if (bool1 ^ bool2) data.ccr.setC else data.ccr.clearC
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (reg & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                val bool = (bool1 ^^ bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }
        }

      case 0x76 =>
        val op1 = rom.getByte(pc + 1)
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
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (reg & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool = (bool1 && bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }

          case 0x80 =>
            //BIAND.B Imm,Reg [76][1imm reg]
            (ccr, reg) match {
              case (c: IntSymbol, r: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (r & 0x01).eq(0x00)
                if (bool1 & bool2) data.ccr.setC else data.ccr.clearC
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (reg & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                val bool = (bool1 && bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }
        }

      case 0x77 =>
        val op1 = rom.getByte(pc + 1)
        val imm = (op1 >> 4) & 0x07
        data.pc.setPc(pc + 2)
        op1 & 0x80 match {
          case 0x00 =>
            //BLD.B Imm,Reg [77][immreg]
            data.reg.getByte(op1) >> imm match {
              case reg: IntSymbol =>
                if ((reg & 0x01).eq(0x01)) data.ccr.clearC else data.ccr.setC
                buf += data
              case reg: CtxSymbol =>
                val bool = (reg & 0x01).eq(0x01)
                val d1 = data.clone
                d1.ccr.clearC
                data.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.setC
                data.path.set(bool.not.symbol)
                buf += d2
            }
          case 0x80 =>
            //BILD.B Imm,Reg [77][1immreg]
            data.reg.getByte(op1) >> imm match {
              case reg: IntSymbol =>
                if ((reg & 0x01).eq(0x00)) data.ccr.setC else data.ccr.clearC
                buf += data
              case reg: CtxSymbol =>
                val bool = (reg & 0x01).eq(0x00)
                val d1 = data.clone
                d1.ccr.setC
                data.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                data.path.set(bool.not.symbol)
                buf += d2

            }
        }

      case 0x79 => analyze79(data, pc)

      case 0x7A => analyze7A(data, pc)

      case 0x7C => analyze7C(data, pc)

      case 0x7D => analyze7D(data, pc)

      case 0x7E => analyze7E(data, pc)

      case 0x7F => analyze7F(data, pc)

    }
    buf
  }

  private def analyze79(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
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

      case 0x60 =>
        //AND.W Imm,Reg [79][6reg][imm][imm]
        val imm = rom.getWord(pc + 2)
        val reg = data.reg.getWord(op1)
        val and = reg & imm
        data.reg.setWord(and, op1)
        data.pc.setPc(pc + 4)
        data.ccr.clearV
        buf = checkZ(and, ArrayBuffer(data))
        buf = checkN(and, buf, 8)
    }
    buf
  }

  private def analyze7A(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    val op1 = rom.getByte(pc + 1)
    val imm = new IntSymbol(rom.getLong(pc + 2))
    op1 & 0xF0 match {
      case 0x00 =>
        //MOV,L Imm,Reg [7A][0reg][imm][imm][imm][imm]
        data.reg.setLong(imm, op1)
        data.pc.setPc(pc + 6)
        data.ccr.clearV
        buf = checkZ(imm, ArrayBuffer(data))
        buf = checkN(imm, buf, 32)

      case 0x60 =>
        //AND.L Imm,Reg [7A][6reg][imm][imm][imm][imm]
        val reg = data.reg.getLong(op1)
        val and = reg & imm
        data.reg.setLong(and, op1)
        data.pc.setPc(pc + 6)
        data.ccr.clearV
        buf = checkZ(and, ArrayBuffer(data))
        buf = checkN(and, buf, 8)
    }
    buf
  }

  private def analyze7C(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
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
                    buf += data
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                    val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                    val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
                    val d1 = data.clone
                    d1.ccr.setC
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.ccr.clearC
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case reg: CtxSymbol =>
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach {
                  p =>
                    val clone = data.clone
                    val mem = clone.mem.getByte(p) >> imm
                    clone.path.set(reg.extract(15, 0).eq(p).symbol)
                    (ccr, mem) match {
                      case (c: IntSymbol, m: IntSymbol) =>
                        val bool1 = (c & 0x01).eq(0x01)
                        val bool2 = (m & 0x01).eq(0x01)
                        if (bool1 | bool2) clone.ccr.setC else clone.ccr.clearC
                        buf += clone
                      case _ =>
                        val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                        val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                        val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
                        val d1 = clone.clone
                        d1.ccr.setC
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.ccr.clearC
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
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
                    buf += data
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                    val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                    val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
                    val d1 = data.clone
                    d1.ccr.setC
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.ccr.clearC
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case reg: CtxSymbol =>
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach {
                  p =>
                    val clone = data.clone
                    val mem = clone.mem.getByte(p) >> imm
                    clone.path.set(reg.extract(15, 0).eq(p).symbol)
                    (ccr, mem) match {
                      case (c: IntSymbol, m: IntSymbol) =>
                        val bool1 = (c & 0x01).eq(0x01)
                        val bool2 = (m & 0x01).eq(0x00)
                        if (bool1 | bool2) clone.ccr.setC else clone.ccr.clearC
                        buf += clone
                      case _ =>
                        val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                        val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                        val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
                        val d1 = clone.clone
                        d1.ccr.setC
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.ccr.clearC
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
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
                    buf += data
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                    val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                    val bool = (bool1 ^^ bool2).asInstanceOf[CtxSymbol]
                    val d1 = data.clone
                    d1.ccr.setC
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.ccr.clearC
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case reg: CtxSymbol =>
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach {
                  p =>
                    val clone = data.clone
                    val mem = clone.mem.getByte(p) >> imm
                    clone.path.set(reg.extract(15, 0).eq(p).symbol)
                    (ccr, mem) match {
                      case (c: IntSymbol, m: IntSymbol) =>
                        val bool1 = (c & 0x01).eq(0x01)
                        val bool2 = (m & 0x01).eq(0x01)
                        if (bool1 ^ bool2) clone.ccr.setC else clone.ccr.clearC
                        buf += clone
                      case _ =>
                        val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                        val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                        val bool = (bool1 ^^ bool2).asInstanceOf[CtxSymbol]
                        val d1 = clone.clone
                        d1.ccr.setC
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.ccr.clearC
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
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
                    buf += data
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                    val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                    val bool = (bool1 ^^ bool2).asInstanceOf[CtxSymbol]
                    val d1 = data.clone
                    d1.ccr.setC
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.ccr.clearC
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case reg: CtxSymbol =>
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach {
                  p =>
                    val clone = data.clone
                    val mem = clone.mem.getByte(p) >> imm
                    clone.path.set(reg.extract(15, 0).eq(p).symbol)
                    (ccr, mem) match {
                      case (c: IntSymbol, m: IntSymbol) =>
                        val bool1 = (c & 0x01).eq(0x01)
                        val bool2 = (m & 0x01).eq(0x00)
                        if (bool1 ^ bool2) clone.ccr.setC else clone.ccr.clearC
                        buf += clone
                      case _ =>
                        val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                        val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                        val bool = (bool1 ^^ bool2).asInstanceOf[CtxSymbol]
                        val d1 = clone.clone
                        d1.ccr.setC
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.ccr.clearC
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
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
                    buf += data
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                    val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                    val bool = (bool1 && bool2).asInstanceOf[CtxSymbol]
                    val d1 = data.clone
                    d1.ccr.setC
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.ccr.clearC
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case reg: CtxSymbol =>
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach {
                  p =>
                    val clone = data.clone
                    val mem = clone.mem.getByte(p) >> imm
                    clone.path.set(reg.extract(15, 0).eq(p).symbol)
                    (ccr, mem) match {
                      case (c: IntSymbol, m: IntSymbol) =>
                        val bool1 = (c & 0x01).eq(0x01)
                        val bool2 = (m & 0x01).eq(0x01)
                        if (bool1 & bool2) clone.ccr.setC else clone.ccr.clearC
                        buf += clone
                      case _ =>
                        val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                        val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                        val bool = (bool1 && bool2).asInstanceOf[CtxSymbol]
                        val d1 = clone.clone
                        d1.ccr.setC
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.ccr.clearC
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
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
                    buf += data
                  case _ =>
                    val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                    val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                    val bool = (bool1 && bool2).asInstanceOf[CtxSymbol]
                    val d1 = data.clone
                    d1.ccr.setC
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.ccr.clearC
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case reg: CtxSymbol =>
                extract(0 to 0xFFFF, reg.extract(15, 0).symbol).foreach {
                  p =>
                    val clone = data.clone
                    val mem = clone.mem.getByte(p) >> imm
                    clone.path.set(reg.extract(15, 0).eq(p).symbol)
                    (ccr, mem) match {
                      case (c: IntSymbol, m: IntSymbol) =>
                        val bool1 = (c & 0x01).eq(0x01)
                        val bool2 = (m & 0x01).eq(0x00)
                        if (bool1 & bool2) clone.ccr.setC else clone.ccr.clearC
                        buf += clone
                      case _ =>
                        val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                        val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                        val bool = (bool1 && bool2).asInstanceOf[CtxSymbol]
                        val d1 = clone.clone
                        d1.ccr.setC
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.ccr.clearC
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
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
                    buf += data
                  case m: CtxSymbol =>
                    val bool = (m & 0x01).eq(0x01)
                    val d1 = data.clone
                    d1.ccr.setC
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.ccr.clearC
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach {
                  p =>
                    val clone = data.clone
                    clone.path.set(ind.extract(15, 0).eq(p).symbol)
                    clone.mem.getByte(p) >> imm match {
                      case m: IntSymbol =>
                        if ((m & 0x01).eq(0x01)) clone.ccr.setC else clone.ccr.clearC
                        buf += clone
                      case m: CtxSymbol =>
                        val bool = (m & 0x01).eq(0x01)
                        val d1 = clone.clone
                        d1.ccr.setC
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.ccr.clearC
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
            }

          case 0x80 =>
            //BILD.B Imm,IndReg [7C][indreg0][77][1imm0]
            data.reg.getLong(rom.getByte(pc + 1) >> 4) match {
              case ind: IntSymbol =>
                data.mem.getByte(ind.symbol) >> imm match {
                  case m: IntSymbol =>
                    if ((m & 0x01).eq(0x00)) data.ccr.setC else data.ccr.clearC
                    buf += data
                  case m: CtxSymbol =>
                    val bool = (m & 0x01).eq(0x00)
                    val d1 = data.clone
                    d1.ccr.setC
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.ccr.clearC
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case ind: CtxSymbol =>
                extract(0 to 0xFFFF, ind.extract(15, 0)).foreach {
                  p =>
                    val clone = data.clone
                    clone.path.set(ind.extract(15, 0).eq(p).symbol)
                    clone.mem.getByte(p) >> imm match {
                      case m: IntSymbol =>
                        if ((m & 0x01).eq(0x00)) clone.ccr.setC else clone.ccr.clearC
                        buf += clone
                      case m: CtxSymbol =>
                        val bool = (m & 0x01).eq(0x00)
                        val d1 = clone.clone
                        d1.ccr.setC
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.ccr.clearC
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
            }
        }
    }
    buf
  }

  private def analyze7D(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    val buf = new ArrayBuffer[DataSet]
    rom.getByte(pc + 2) match {
      case 0x62 =>
        //BCLR.B Reg,IndReg [7D][indreg0][62][reg0]
        val reg = data.reg.getByte(rom.getByte(pc + 3) >> 4) & 0x07
        data.pc.setPc(pc + 4)
        data.reg.getByte(rom.getByte(pc + 1) >> 4) match {
          case ind: IntSymbol =>
            val mem = data.mem.getByte(ind.symbol)
            val bclr = reg.bitclr(reg)
            data.mem.setByte(bclr, ind.symbol)
            buf += data
          case ind: CtxSymbol =>
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach {
              p =>
                val clone = data.clone
                val mem = clone.mem.getByte(p)
                clone.path.set(ind.extract(15, 0).eq(p).symbol)
                val bclr = reg.bitclr(reg)
                clone.mem.setByte(bclr, p)
                buf += clone
            }
        }

      case 0x67 =>
        val op3 = rom.getByte(pc + 3)
        val imm = (op3 >> 4) & 0x07
        val ccr = data.ccr.ccr
        op3 & 0x80 match {
          case 0x00 =>
            //BST.B Imm,Indreg [7D][reg0][67][imm0]
            data.reg.getByte(rom.getByte(pc + 1) >> 4) match {
              case reg: IntSymbol =>
                val mem = data.mem.getByte(reg.symbol)
                (ccr, mem) match {
                  case (c: IntSymbol, m: IntSymbol) =>
                    val bst = if ((c & 0x01).eq(0x01)) m | (1 << imm) else m & (~(1 << imm))
                    data.mem.setByte(bst, reg.symbol)
                    buf += data
                  case _ =>
                    val bool = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8)).asInstanceOf[CtxSymbol]
                    val d1 = data.clone
                    d1.mem.setByte(mem | (1 << imm), reg.symbol)
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.mem.setByte(mem & (~(1 << imm)), reg.symbol)
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case reg: CtxSymbol =>
                extract(0 to 0xFFFF, reg.extract(15, 0)).foreach {
                  p =>
                    val clone = data.clone
                    clone.path.set(reg.extract(15, 0).eq(p).symbol)
                    val mem = clone.mem.getByte(p)
                    (ccr, mem) match {
                      case (c: IntSymbol, m: IntSymbol) =>
                        val bst = if ((c & 0x01).eq(0x01)) m | (1 << imm) else m & (~(1 << imm))
                        clone.mem.setByte(bst, p)
                        buf += clone
                      case _ =>
                        val bool = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8)).asInstanceOf[CtxSymbol]
                        val d1 = clone.clone
                        d1.mem.setByte(mem | (1 << imm), p)
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.mem.setByte(mem & (~(1 << imm)), p)
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
            }
          case 0x80 =>
            //BIST.B Imm,Indreg [7D][reg0][67][1imm0]]
            data.reg.getByte(rom.getByte(pc + 1) >> 4) match {
              case reg: IntSymbol =>
                val mem = data.mem.getByte(reg.symbol)
                (ccr, mem) match {
                  case (c: IntSymbol, m: IntSymbol) =>
                    val bst = if ((c & 0x01).eq(0x00)) m | (1 << imm) else m & (~(1 << imm))
                    data.mem.setByte(bst, reg.symbol)
                    buf += data
                  case _ =>
                    val bool = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8)).asInstanceOf[CtxSymbol]
                    val d1 = data.clone
                    d1.mem.setByte(mem | (1 << imm), reg.symbol)
                    d1.path.set(bool.symbol)
                    buf += d1
                    val d2 = data.clone
                    d2.mem.setByte(mem & (~(1 << imm)), reg.symbol)
                    d2.path.set(bool.not.symbol)
                    buf += d2
                }
              case reg: CtxSymbol =>
                extract(0 to 0xFFFF, reg.extract(15, 0)).foreach {
                  p =>
                    val clone = data.clone
                    clone.path.set(reg.extract(15, 0).eq(p).symbol)
                    val mem = clone.mem.getByte(p)
                    (ccr, mem) match {
                      case (c: IntSymbol, m: IntSymbol) =>
                        val bst = if ((c & 0x01).eq(0x00)) m | (1 << imm) else m & (~(1 << imm))
                        clone.mem.setByte(bst, p)
                        buf += clone
                      case _ =>
                        val bool = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8)).asInstanceOf[CtxSymbol]
                        val d1 = clone.clone
                        d1.mem.setByte(mem | (1 << imm), p)
                        d1.path.set(bool.symbol)
                        buf += d1
                        val d2 = clone.clone
                        d2.mem.setByte(mem & (~(1 << imm)), p)
                        d2.path.set(bool.not.symbol)
                        buf += d2
                    }
                }
            }
        }

      case 0x71 =>
        //BNOT.B Imm,IndReg [7D][immreg][0x71][imm0]
        val ind = data.reg.getByte(rom.getByte(pc + 1) >> 4)
        val imm = rom.getByte(pc + 3) >> 4
        data.pc.setPc(pc + 2)
        data.reg.getByte(rom.getByte(pc + 1) >> 4) match {
          case ind: IntSymbol =>
            data.mem.getByte(ind.symbol) >> imm match {
              case m: IntSymbol =>
                val bnot = if ((m & 0x01).eq(0x01)) m.bitclr(imm) else m.bitset(imm)
                data.reg.setByte(bnot, ind.symbol)
                buf += data
              case m: CtxSymbol =>
                val bool = (m & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8)).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.reg.setByte(m.bitclr(imm), ind.symbol)
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.reg.setByte(m.bitset(imm), ind.symbol)
                d2.path.set(bool.not.symbol)
                buf += d2
            }
          case ind: CtxSymbol =>
            extract(0 to 0xFFFF, ind.extract(15, 0)).foreach { p =>
              val clone = data.clone
              clone.path.set(ind.extract(15, 0).eq(p).symbol)
              clone.mem.getByte(p) >> imm match {
                case m: IntSymbol =>
                  val bnot = if ((m & 0x01).eq(0x01)) m.bitclr(imm) else m.bitset(imm)
                  clone.reg.setByte(bnot, p)
                  buf += clone
                case m: CtxSymbol =>
                  val bool = (m & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8)).asInstanceOf[CtxSymbol]
                  val d1 = clone.clone
                  d1.reg.setByte(m.bitclr(imm), p)
                  d1.path.set(bool.symbol)
                  buf += d1
                  val d2 = clone.clone
                  d2.reg.setByte(m.bitset(imm), p)
                  d2.path.set(bool.not.symbol)
                  buf += d2
              }
            }
        }

      case 0x72 =>
        //BCLR.B Imm,Indreg [7D][reg0][72][imm0]
        val imm = rom.getByte(pc + 3) >> 4
        val op1 = rom.getByte(pc + 1) >> 4
        data.pc.setPc(pc + 4)
        data.reg.getLong(op1) match {
          case reg: IntSymbol =>
            val mem = data.mem.getByte(reg.symbol)
            val bclr = reg.bitclr(imm)
            data.mem.setByte(bclr, reg.symbol)
            buf += data
          case reg: CtxSymbol =>
            extract(0 to 0xFFFF, reg.extract(15, 0)).foreach {
              p =>
                val clone = data.clone
                val mem = clone.mem.getByte(p)
                clone.path.set(reg.extract(15, 0).eq(p).symbol)
                val bclr = reg.bitclr(imm)
                clone.mem.setByte(bclr, p)
                buf += clone
            }
        }
    }
    buf
  }

  private def analyze7E(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
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
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }

          case 0x80 =>
            //BIOR.B Imm,Abs [7E][abs][74][1imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (m & 0x01).eq(0x00)
                if (bool1 | bool2) data.ccr.setC else data.ccr.clearC
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                val bool = (bool1 || bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
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
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool = (bool1 ^^ bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }

          case 0x80 =>
            //BIXOR.B Imm,Abs [7E][abs][75][1imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (m & 0x01).eq(0x00)
                if (bool1 ^ bool2) data.ccr.setC else data.ccr.clearC
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                val bool = (bool1 ^^ bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
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
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool = (bool1 && bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }

          case 0x80 =>
            //BIAND.B Imm,Abs [7E][abs][76][1imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bool1 = (c & 0x01).eq(0x01)
                val bool2 = (m & 0x01).eq(0x00)
                if (bool1 & bool2) data.ccr.setC else data.ccr.clearC
                buf += data
              case _ =>
                val bool1 = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8))
                val bool2 = (mem & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8))
                val bool = (bool1 && bool2).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }
        }

      case 0x77 =>
        op3 & 0x80 match {
          case 0x00 =>
            //BLD.B Imm,Abs [7E][abs][77][imm0]
            data.mem.getByte(rom.getByte(pc + 1)) >> imm match {
              case mem: IntSymbol =>
                if ((mem & 0x01).eq(0x01)) data.ccr.setC else data.ccr.clearC
                buf += data
              case mem: CtxSymbol =>
                val bool = (mem & 0x01).eq(0x01)
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }

          case 0x80 =>
            //BILD.B Imm,Abs [7E][abs][77][1imm0]
            data.mem.getByte(rom.getByte(pc + 1)) >> imm match {
              case mem: IntSymbol =>
                if ((mem & 0x01).eq(0x00)) data.ccr.setC else data.ccr.clearC
                buf += data
              case mem: CtxSymbol =>
                val bool = (mem & 0x01).eq(0x00)
                val d1 = data.clone
                d1.ccr.setC
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.ccr.clearC
                d2.path.set(bool.not.symbol)
                buf += d2
            }
        }
    }
    buf
  }

  private def analyze7F(data: DataSet, pc: Int): ArrayBuffer[DataSet] = {
    var buf = new ArrayBuffer[DataSet]
    rom.getByte(pc + 2) match {
      case 0x62 =>
        //BCLR.B Reg,Abs [7F][abs][62][reg0]
        val abs = rom.getByte(pc + 1) | 0xFFFFFF00
        val reg = data.reg.getByte(rom.getByte(pc + 3) >> 4) & 0x07
        val bclr = data.mem.getByte(abs).bitclr(reg)
        data.mem.setByte(bclr, abs)
        data.pc.setPc(pc + 4)
        buf += data

      case 0x67 =>
        val op3 = rom.getByte(pc + 3)
        val abs = rom.getByte(pc + 1) | 0xFFFFFF00
        val imm = (op3 >> 4) & 0x07
        val mem = data.mem.getByte(abs)
        val ccr = data.ccr.ccr
        op3 & 0x80 match {
          case 0x00 =>
            //BST.B Imm,Abs [7F][abs][67][Imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bst = if ((c & 0x01).eq(0x01)) m | (1 << imm) else m & (~(1 << imm))
                data.mem.setByte(bst, abs)
                buf += data
              case _ =>
                val bool = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x01, 8)).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.mem.setByte(mem | (1 << imm), abs)
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.mem.setByte(mem & (~(1 << imm)), abs)
                d2.path.set(bool.not.symbol)
                buf += d2
            }

          case 0x80 =>
            //BIST.B Imm,Abs [7F][abs][67][1Imm0]
            (ccr, mem) match {
              case (c: IntSymbol, m: IntSymbol) =>
                val bst = if ((c & 0x01).eq(0x00)) m | (1 << imm) else m & (~(1 << imm))
                data.mem.setByte(bst, abs)
                buf += data
              case _ =>
                val bool = (ccr & new CtxSymbol(ctx, 0x01, 8)).eq(new CtxSymbol(ctx, 0x00, 8)).asInstanceOf[CtxSymbol]
                val d1 = data.clone
                d1.mem.setByte(mem | (1 << imm), abs)
                d1.path.set(bool.symbol)
                buf += d1
                val d2 = data.clone
                d2.mem.setByte(mem & (~(1 << imm)), abs)
                d2.path.set(bool.not.symbol)
                buf += d2
            }
        }

      case 0x70 =>
        //BSET.B Imm,Abs [7F][abs][70][Imm0]
        val abs = rom.getByte(pc + 1) | 0xFFFFFF00
        val imm = rom.getByte(pc + 3) >> 4
        val bset = data.mem.getByte(abs).bitset(imm)
        data.mem.setByte(bset, abs)
        data.pc.setPc(pc + 4)
        buf += data

      case 0x72 =>
        //BSET.B Imm,Abs [7F][abs][72][Imm0]
        val abs = rom.getByte(pc + 1) | 0xFFFFFF00
        val imm = rom.getByte(pc + 3) >> 4
        val bclr = data.mem.getByte(abs).bitclr(imm)
        data.mem.setByte(bclr, abs)
        data.pc.setPc(pc + 4)
        buf += data
    }
    buf
  }

  private def extract(range: Range, ast: Z3AST): ArrayBuffer[Int] = {
    val s = ctx.mkSolver
    val buf = new ArrayBuffer[Int]
    range.foreach { n =>
      s.push
      s.assertCnstr(ctx.mkEq(ast, ctx.mkInt(n, ast.getSort)))
      if (s.check.get) buf += n
      s.pop(1)
    }
    buf
  }

  def extract(range: Range, c: CtxSymbol): ArrayBuffer[Int] = extract(range, c.symbol)

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
          val bool = bool1 | bool2
          if (bool) b.ccr.setC else b.ccr.clearC
          ans += b
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
          val and = size match {
            case 8 => 0x80
            case 16 => 0x8000
            case 32 => 0x80000000
          }
          val bool1 = (d1 & and).eq(and) && (d2 & and).eq(and) && (r & and).eq(0)
          val bool2 = (d1 & and).eq(0) && (d2 & and).eq(0) && (r & and).eq(and)
          val bool = bool1 | bool2
          if (bool) b.ccr.setV else b.ccr.clearV
          ans += b
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
          if (d == 0) b.ccr.setZ else b.ccr.clearZ
          ans += b
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
          val and = size match {
            case 8 => 0x80
            case 16 => 0x8000
            case 32 => 0x80000000
          }
          if ((d & and).eq(and)) b.ccr.setN else b.ccr.clearN
          ans += b
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
          val bool = bool1 | bool2
          if (bool) b.ccr.setH else b.ccr.clearH
          ans += b
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
