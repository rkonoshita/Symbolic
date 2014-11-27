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
                buf = checkZ(reg, buf)
                buf = checkN(reg, buf)
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
                buf = checkZ(reg, buf)
                buf = checkN(reg, buf)
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
                val bset =
                  data.mem.getByte(abs) match {
                    case b: IntSymbol => b.bitset(imm)
                    case b: CtxSymbol => b.bitset(imm)
                  }
                data.mem.setByte(bset, abs)
            }
        }
      case 0xF0 =>
        //MOV.B Imm,Reg [Freg][Imm]
        val imm = data.mem.getByte(pc + 1)
        data.reg.setByte(imm, op0)
        data.ccr.ccr = data.ccr.clearV
        buf = checkZ(imm, buf)
        buf = checkN(imm, buf)
    }
    buf
  }

  def checkZ(data: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b =>
      val clist = b.ccr.checkZ(data, size)
      //長さは必ず1か2
      clist.length match {
        case 1 => //記号でも自明な場合や、数値である場合はこっち
          val d = b.clone
          d.ccr.ccr = clist(0)
          ans += d
        case 2 => //こっちになるのはdataがCtxSymbolの時のみ（のはず）
          //第一波:true 第二波:false
          val sym = data.asInstanceOf[CtxSymbol].symbol
          val d1 = b.clone
          d1.ccr.ccr = clist(0)
          d1.path.set(ctx.mkEq(sym, ctx.mkInt(0, sym.getSort)))
          val d2 = b.clone
          d1.ccr.ccr = clist(1)
          d1.path.set(ctx.mkNot(ctx.mkEq(sym, ctx.mkInt(0, sym.getSort))))
      }
    }
    ans
  }

  def checkN(data: MySymbol, buf: ArrayBuffer[DataSet], size: Int): ArrayBuffer[DataSet] = {
    val ans = new ArrayBuffer[DataSet]
    buf.foreach { b =>
      val clist = b.ccr.checkN(data, size)
      //長さは必ず1か2
      clist.length match {
        case 1 => //記号でも自明な場合や、数値である場合はこっち
          val d = b.clone
          d.ccr.ccr = clist(0)
          ans += d
        case 2 => //こっちになるのはdataがCtxSymbolの時のみ（のはず）
          //第一波:true 第二波:false
          val sym = data.asInstanceOf[CtxSymbol].symbol
          val d1 = b.clone
          d1.ccr.ccr = clist(0)
          d1.path.set(ctx.mkNot(ctx.mkBVSge(sym, ctx.mkInt(0, sym.getSort))))
          val d2 = b.clone
          d1.ccr.ccr = clist(1)
          d1.path.set(ctx.mkBVSge(sym, ctx.mkInt(0, sym.getSort)))
      }
    }
    ans
  }

}
