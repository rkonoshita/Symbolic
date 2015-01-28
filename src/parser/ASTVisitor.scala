package parser

import java.io.File

import data.register.ROM
import base.Parameter
import z3.scala.Z3Context

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by rkonoshita on 14/11/20.
 */

//意味解析用
class ASTVisitor {

  val label = new mutable.HashMap[String, Int]
  //ラベルの位置を保持
  val count = new MyHashMap // 各セクションの終点アドレスを保持
  count ++= Parameter.getStart
  var section = ""
  val parseResult = new ListBuffer[AST]
  val tmppc = new MyHashMap
  tmppc ++= Parameter.getStart

  //ここでメモリにオペランドを配置する
  def makeProgram(ctx: Z3Context, file: File): ROM = {
    //構文解析
    file.listFiles.foreach { f => Source.fromFile(f).getLines.foreach { l =>
//      println(l)
      parseResult += new ASTParser().parse(l).get}}

    //意味解析１回目：ラベルの位置を決める
    parseResult.foreach { p =>
      search(p) match {
        case Some(i: Int) => count.put(section, i)
        case _ =>
      }
    }

    val rom = new mutable.HashMap[Int, Byte]
    //意味解析してROM上にデータを配置していく
    parseResult.foreach { p =>
      println(p)
      visit(p) match {
        case Some(array: Array[Int]) =>
          (0 until array.length).foreach { op =>
            print("%x".format(array(op).toByte) + ":")
            rom += (tmppc(section) + op) -> array(op).toByte
          }
          tmppc.put(section, array.length)
        case _ =>
      }
      println
    }
    Parameter.sizeset(count)
    new ROM(rom)
  }

  //ラベル位置の捜索
  //文法的な正しさは度外視
  def search(ast: AST): Option[Any] = {
    ast match {
      case Add(left, right) => Some((left, right) match {
        case (_: Imm, _: RegWord) => 4
        case (_: Imm, _: RegLong) => 4
        case _ => 2
      })

      case AddSign(left, right) => Some(2)

      case AddExtends(left, right) => Some(2)

      case And(left, right) => Some((left, right) match {
        case (_: Imm, _: RegWord) => 4
        case (_: Imm, _: RegLong) => 6
        case (_: RegLong, _: RegLong) => 4
        case _ => 2
      })

      case Andc(item) => Some(2)

      case Band(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Bra(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Brn(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bhi(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bls(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bcc(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bcs(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bne(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Beq(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bvc(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bvs(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bpl(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bmi(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bge(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Blt(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bgt(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Ble(num, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bclr(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case (_: RegByte, _: RegByte) => 2
        case _ => 4
      })

      case Biand(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Bild(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Bior(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Bist(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Bixor(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Bld(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Bnot(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Bor(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Bset(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case (_: RegByte, _: RegByte) => 2
        case _ => 4
      })

      case Bsr(disp, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bst(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Btst(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case (_: RegByte, _: RegByte) => 2
        case _ => 4
      })

      case Bxor(left, right) => Some((left, right) match {
        case (_: Imm, _: RegByte) => 2
        case _ => 4
      })

      case Cmp(left, right) => Some((left, right) match {
        case (_: Imm, _: RegWord) => 4
        case (_: Imm, _: RegLong) => 6
        case _ => 2
      })

      case Daa(reg) => Some(2)

      case Das(reg) => Some(2)

      case Dec(left, right) => Some(2)

      case Divxs(left, right) => Some(4)

      case Divxu(left, right) => Some(2)

      case Eepmov(size) => Some(4)

      case Exts(reg) => Some(2)

      case Extu(reg) => Some(2)

      case Inc(left, right) => Some(2)

      case Jmp(add) => Some(add match {
        case _: AbsAddress => 4
        case _ => 2
      })

      case Jsr(add) => Some(add match {
        case _: AbsAddress => 4
        case _ => 2
      })

      case Ldc(item) => Some(item match {
        case _: IndirReg => 4
        case disp: Disp => search(disp) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 10
          }
        }
        case _: Pos => 4
        case abs: AbsAddress => search(abs) match {
          case Some(s: Int) => s match {
            case 8 => 2
            case 16 => 6
            case 24 => 10
          }
        }
        case _ => 2
      })

      case Mov(left, right) => Some((left, right) match {
        case (l: Disp, _: RegByte) => search(l) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        }
        case (l: AbsAddress, _: RegByte) => search(l) match {
          case Some(s: Int) => s match {
            case 8 => 2
            case 16 => 4
            case 24 => 6
          }
        }
        case (_: Imm, _: RegWord) => 4
        case (l: Disp, _: RegWord) => search(l) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        }
        case (l: AbsAddress, _: RegWord) => search(l) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        }
        case (_: Imm, _: RegLong) => 6
        case (_: IndirReg, _: RegLong) => 4
        case (l: Disp, _: RegLong) => search(l) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 10
          }
        }
        case (_: Pos, _: RegLong) => 4
        case (l: AbsAddress, _: RegLong) => search(l) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 8
          }
        }
        case (_: RegByte, r: Disp) => search(r) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        }
        case (_: RegByte, r: AbsAddress) => search(r) match {
          case Some(s: Int) => s match {
            case 8 => 2
            case 16 => 4
            case 24 => 6
          }
        }
        case (_: RegWord, r: Disp) => search(r) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        }
        case (_: RegWord, r: AbsAddress) => search(r) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 6
          }
        }
        case (_: RegLong, _: IndirReg) => 4
        case (_: RegLong, r: Disp) => search(r) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 10
          }
        }
        case (_: RegLong, _: Pre) => 4
        case (_: RegLong, r: AbsAddress) => search(r) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 6
          }
        }
        case _ => 2
      })

      case Movfpe(lefr, right) => Some(4)

      case Movtpe(left, right) => Some(4)

      case Mulxs(left, right) => Some(4)

      case Mulxu(left, right) => Some(2)

      case Neg(reg) => Some(2)

      case Nop() => Some(2)

      case Not(item) => Some(2)

      case Or(left, right) => Some((left, right) match {
        case (_: Imm, _: RegWord) => 4
        case (_: Imm, _: RegLong) => 6
        case (_: RegLong, _: RegLong) => 4
        case _ => 2
      })

      case Orc(item) => Some(2)

      case Pop(reg) => Some(reg match {
        case (_: RegWord) => 2
        case (_: RegLong) => 4
      })

      case Push(reg) => Some(reg match {
        case (_: RegWord) => 2
        case (_: RegLong) => 4
      })

      case Rotl(reg) => Some(2)

      case Rotr(reg) => Some(2)

      case Rotxl(reg) => Some(2)

      case Rotxr(reg) => Some(2)

      case Rte() => Some(2)

      case Rts() => Some(2)

      case Shal(reg) => Some(2)

      case Shar(reg) => Some(2)

      case Shll(reg) => Some(2)

      case Shlr(reg) => Some(2)

      case Sleep() => Some(2)

      case Stc(reg) => Some(reg match {
        case _: RegByte => 2
        case r: Disp => search(r) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 10
          }
        }
        case r: AbsAddress => search(r) match {
          case Some(s: Int) => s match {
            case 8 => 2
            case 16 => 6
            case 24 => 8
          }
        }
        case _ => 4
      })

      case Sub(left, right) => Some((left, right) match {
        case (_: Imm, _: RegWord) => 4
        case (_: Imm, _: RegLong) => 6
        case _ => 2
      })

      case Subs(left, right) => Some(2)

      case Subx(left, right) => Some(2)

      case Trapa(imm) => Some(2)

      case Xor(left, right) => Some((left, right) match {
        case (_: Imm, _: RegWord) => 4
        case (_: Imm, _: RegLong) => 6
        case (_: RegLong, _: RegLong) => 4
        case _ => 2
      })

      case Xorc(imm) => Some(2)

      case Data(num, size) => Some(size match {
        case 8 => 1
        case 16 => 2
        case 32 => 4
      })

      case DataBlock(block, data, size) =>
        Some(search(block) match {
          case Some(s: Int) => s * (size match {
            case 8 => 1
            case 16 => 2
            case 32 => 4
          })
        })

      case AbsAddress(num, size) => Some(size)

      case Disp(disp, reg, size) => Some(size)

      case LabelName(str) => Some(str)

      case Number(num) => Some(num)

      case MakeLabel(name) => label += (search(name) match {
        case Some(s: String) => s -> count(section)
      })
        None

      case Section(sec) =>
        section = sec
        None
    }
  }

  //メモリに命令やデータを配置
  def visit(ast: AST): Option[Any] = {
    ast match {
      case Add(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x80 | r, l)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x08, (l << 4) | r)
        }
        case (imm: Imm, reg: RegWord) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, 0x10 | r, l >> 8, l)
        }
        case (lreg: RegWord, rreg: RegWord) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x09, (l << 4) | r)
        }
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, 0x10 | r, l >> 24, l >> 16, l >> 8, l)
        }
        case (lreg: RegLong, rreg: RegLong) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0A, 0x80 | (l << 4) | r)
        }
      })

      case AddSign(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0B, r | (l match {
            case 1 => 0x00
            case 2 => 0x80
            case 4 => 0x90
          }))
        }
      })

      case AddExtends(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x90 | r, l)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0E, (l << 4) | r)
        }
      })

      case And(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0xE0 | r, l)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x16, (l << 4) | r)
        }
        case (imm: Imm, reg: RegWord) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, 0x60 | r, l >> 8, l)
        }
        case (lreg: RegWord, rreg: RegWord) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x66, (l << 4) | r)
        }
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, 0x60 | r, l >> 24, l >> 16, l >> 8, l)
        }
        case (lreg: RegLong, reg: RegLong) => (visit(lreg), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x01, 0xF0, 0x66, (l << 4) | r)
        }
      })

      case Andc(item) => Some(item match {
        case (imm: Imm) => visit(imm) match {
          case Some(i: Int) => Array(0x06, i)
        }
      })

      case Band(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x76, (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x76, l << 4)
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x76, l << 4)
        }
      })

      case Bra(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x40, disp)
            case 16 => Array(0x58, 0x00, disp >> 8, disp)
          }
      })

      case Brn(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x41, disp)
            case 16 => Array(0x58, 0x10, disp >> 8, disp)
          }
      })

      case Bhi(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x42, disp)
            case 16 => Array(0x58, 0x20, disp >> 8, disp)
          }
      })

      case Bls(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x43, disp)
            case 16 => Array(0x58, 0x30, disp >> 8, disp)
          }
      })

      case Bcc(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x44, disp)
            case 16 => Array(0x58, 0x40, disp >> 8, disp)
          }
      })

      case Bcs(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x45, disp)
            case 16 => Array(0x58, 0x50, disp >> 8, disp)
          }
      })

      case Bne(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x46, disp)
            case 16 => Array(0x58, 0x60, disp >> 8, disp)
          }
      })

      case Beq(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x47, disp)
            case 16 => Array(0x58, 0x70, disp >> 8, disp)
          }
      })

      case Bvc(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x48, disp)
            case 16 => Array(0x58, 0x80, disp >> 8, disp)
          }
      })

      case Bvs(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x49, disp)
            case 16 => Array(0x58, 0x90, disp >> 8, disp)
          }
      })

      case Bpl(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x4A, disp)
            case 16 => Array(0x58, 0xA0, disp >> 8, disp)
          }
      })

      case Bmi(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x4B, disp)
            case 16 => Array(0x58, 0xB0, disp >> 8, disp)
          }
      })

      case Bge(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x4C, disp)
            case 16 => Array(0x58, 0xC0, disp >> 8, disp)
          }
      })

      case Blt(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x4D, disp)
            case 16 => Array(0x58, 0xD0, disp >> 8, disp)
          }
      })

      case Bgt(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x4E, disp)
            case 16 => Array(0x58, 0xE0, disp >> 8, disp)
          }
      })

      case Ble(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x4F, disp)
            case 16 => Array(0x58, 0xF0, disp >> 8, disp)
          }
      })

      case Bclr(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x72, (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7D, r << 4, 0x72, l << 4)
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7F, a, 0x72, l << 4)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x62, (l << 4) | r)
        }
        case (lreg: RegByte, rreg: IndirReg) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7D, r << 4, 0x62, l << 4)
        }
        case (reg: RegByte, abs: AbsAddress) => (visit(reg), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7F, a, 0x62, l << 4)
        }
      })

      case Biand(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x76, 0x80 | (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x76, 0x80 | (l << 4))
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x76, 0x80 | (l << 4))
        }
      })

      case Bild(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x77, 0x80 | (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x77, 0x80 | (l << 4))
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x77, 0x80 | (l << 4))
        }
      })

      case Bior(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x74, 0x80 | (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x74, 0x80 | (l << 4))
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x74, 0x80 | (l << 4))
        }
      })

      case Bist(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x67, 0x80 | (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7D, r << 4, 0x67, 0x80 | (l << 4))
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7F, a, 0x67, 0x80 | (l << 4))
        }
      })

      case Bixor(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x75, 0x80 | (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x75, 0x80 | (l << 4))
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x75, 0x80 | (l << 4))
        }
      })

      case Bld(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x77, (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x77, l << 4)
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x77, l << 4)
        }
      })

      case Bnot(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x71, (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7D, r << 4, 0x71, l << 4)
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7F, a, 0x71, l << 4)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x61, (l << 4) | r)
        }
        case (lreg: RegByte, rreg: IndirReg) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7D, r << 4, 0x61, l << 4)
        }
        case (reg: RegByte, abs: AbsAddress) => (visit(reg), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7F, a, 0x61, l << 4)
        }
      })

      case Bor(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x74, (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x74, l << 4)
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x74, l << 4)
        }
      })

      case Bset(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x70, (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7D, r << 4, 0x70, l << 4)
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7F, a, 0x70, l << 4)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x60, (l << 4) | r)
        }
        case (lreg: RegByte, rreg: IndirReg) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7D, r << 4, 0x60, l << 4)
        }
        case (reg: RegByte, abs: AbsAddress) => (visit(reg), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7F, a, 0x60, l << 4)
        }
      })

      case Bsr(num, size) => Some(visit(num) match {
        case Some(n: Int) =>
          val disp = n - tmppc("P")
          size match {
            case 8 => Array(0x55, disp)
            case 16 => Array(0x5C, 0x00, disp >> 8, disp)
          }
      })

      case Bst(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x67, (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7D, r << 4, 0x67, l << 4)
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7F, a, 0x67, l << 4)
        }
      })

      case Btst(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x73, (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x73, l << 4)
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x73, l << 4)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x63, (l << 4) | r)
        }
        case (lreg: RegByte, rreg: IndirReg) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x63, l << 4)
        }
        case (reg: RegByte, abs: AbsAddress) => (visit(reg), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x63, l << 4)
        }
      })

      case Bxor(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x75, (l << 4) | r)
        }
        case (imm: Imm, reg: IndirReg) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7C, r << 4, 0x75, l << 4)
        }
        case (imm: Imm, abs: AbsAddress) => (visit(imm), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x7E, a, 0x75, l << 4)
        }
      })

      case Cmp(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0xA0 | r, l)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1C, (l << 4) | r)
        }
        case (imm: Imm, reg: RegWord) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, 0x20 | r, l >> 8, l)
        }
        case (lreg: RegWord, rreg: RegWord) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1D, (l << 4) | r)
        }
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, 0x20 | r, l >> 24, l >> 16, l >> 8, l)
        }
        case (lreg: RegLong, rreg: RegLong) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1F, 0x80 | (l << 4) | r)
        }
      })

      case Daa(reg) => Some(reg match {
        case r: RegByte => visit(r) match {
          case Some(n: Int) => Array(0x0F, n)
        }
      })

      case Das(reg) => Some(reg match {
        case r: RegByte => visit(r) match {
          case Some(n: Int) => Array(0x1F, n)
        }
      })

      case Dec(left, right) => Some((left, right) match {
        case (reg: RegByte, _: Empty) => visit(reg) match {
          case Some(r: Int) => Array(0x1A, r)
        }
        case (imm: Imm, reg: RegWord) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1B, r | (l match {
            case 1 => 0x50
            case 2 => 0xD0
          }))
        }
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1B, r | (l match {
            case 1 => 0x70
            case 2 => 0xF0
          }))
        }
      })

      case Divxs(left, right) => Some((visit(left), visit(right)) match {
        case (Some(l: Int), Some(r: Int)) => Array(0x01, 0x00, (left, right) match {
          case (lreg: RegByte, rreg: RegWord) => 0x51
          case (lreg: RegWord, rreg: RegLong) => 0x53
        }, (l << 4) | r)
      })

      case Divxu(left, right) => Some((visit(left), visit(right)) match {
        case (Some(l: Int), Some(r: Int)) => Array((left, right) match {
          case (lreg: RegByte, rreg: RegWord) => 0x51
          case (lreg: RegWord, rreg: RegLong) => 0x53
        }, (l << 4) | r)
      })

      case Eepmov(size) => Some(Array(0x7B, size match {
        case 8 => 0x5C
        case 16 => 0xD4
      }, 0x59, 0x8F))

      case Exts(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x17, r | (reg match {
          case r: RegWord => 0xD0
          case r: RegLong => 0xF0
        }))
      })

      case Extu(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x17, r | (reg match {
          case r: RegWord => 0x50
          case r: RegLong => 0x70
        }))
      })

      case Inc(left, right) => Some((left, right) match {
        case (reg: RegByte, _: Empty) => visit(reg) match {
          case Some(r: Int) => Array(0x0A, r)
        }
        case (imm: Imm, reg: RegWord) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0B, r | (l match {
            case 1 => 0x50
            case 2 => 0xD0
          }))
        }
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0B, r | (l match {
            case 1 => 0x70
            case 2 => 0xF0
          }))
        }
      })

      case Jmp(add) => Some(add match {
        case (reg: IndirReg) => visit(reg) match {
          case Some(r: Int) => Array(0x59, r << 4)
        }
        case (abs: AbsAddress) => visit(abs) match {
          case Some((a: Int, _: Int)) => Array(0x5A, a >> 16, a >> 8, a)
        }
        case (abs: IndirAdd) => visit(abs) match {
          case Some(a: Int) => Array(0x5B, a)
        }
      })

      case Jsr(add) => Some(add match {
        case (reg: IndirReg) => visit(reg) match {
          case Some(r: Int) => Array(0x5D, r << 4)
        }
        case (abs: AbsAddress) => visit(abs) match {
          case Some((a: Int, _: Int)) => Array(0x5E, a >> 16, a >> 8, a)
        }
        case (abs: IndirAdd) => visit(abs) match {
          case Some(a: Int) => Array(0x5F, a)
        }
      })

      case Ldc(item) => Some(item match {
        case imm: Imm => visit(imm) match {
          case Some(i: Int) => Array(0x07, i)
        }
        case reg: RegByte => visit(reg) match {
          case Some(r: Int) => Array(0x03, r)
        }
        case reg: IndirReg => visit(reg) match {
          case Some(r: Int) => Array(0x01, 0x40, 0x69, r << 4)
        }
        case disp: Disp => visit(disp) match {
          case Some((d: Int, r: Int, s: Int)) => s match {
            case 16 => Array(0x01, 0x40, 0x6F, r << 4, d >> 8, d)
            case 24 => Array(0x01, 0x40, 0x78, r << 4, 0x6B, 0x20, 0x00, d >> 16, d >> 8, d)
          }
        }
        case pos: Pos => visit(pos) match {
          case Some(r: Int) => Array(0x01, 0x40, 0x6D, r << 4)
        }
        case abs: AbsAddress => visit(abs) match {
          case Some((a: Int, s: Int)) => s match {
            case 16 => Array(0x01, 0x40, 0x6B, 0x00, a >> 8, a)
            case 24 => Array(0x01, 0x40, 0x6B, 0x20, 0x00, a >> 16, a >> 8, a)
          }
        }
      })

      case Mov(left, right) => Some((left, right) match {
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0C, (l << 4) | r)
        }
        case (lreg: RegWord, rreg: RegWord) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0D, (l << 4) | r)
        }
        case (lreg: RegLong, rreg: RegLong) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0F, 0x80 | (l << 4) | r)
        }
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0xF0 | r, l)
        }
        case (lreg: IndirReg, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x68, (l << 4) | r)
        }
        case (disp: Disp, reg: RegByte) => (visit(disp), visit(reg)) match {
          case (Some((d: Int, l: Int, s: Int)), Some(r: Int)) => s match {
            case 16 => Array(0x6E, (l << 4) | r, d >> 8, d)
            case 24 => Array(0x78, l << 4, 0x6A, 0x20 | r, 0x00, d >> 16, d >> 8, d)
          }
        }
        case (pos: Pos, reg: RegByte) => (visit(pos), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x6C, (l << 4) | r)
        }
        case (abs: AbsAddress, reg: RegByte) => (visit(abs), visit(reg)) match {
          case (Some((a: Int, s: Int)), Some(r: Int)) => s match {
            case 8 => Array(0x20 | r, a)
            case 16 => Array(0x6A, r, a >> 8, a)
            case 24 => Array(0x6A, 0x20 | r, 0x00, a >> 16, a >> 8, a)
          }
        }
        case (imm: Imm, reg: RegWord) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, r, l >> 8, l)
        }
        case (lreg: IndirReg, rreg: RegWord) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x69, (l << 4) | r)
        }
        case (disp: Disp, reg: RegWord) => (visit(disp), visit(reg)) match {
          case (Some((d: Int, l: Int, s: Int)), Some(r: Int)) => s match {
            case 16 => Array(0x6F, (l << 4) | r, d >> 8, d)
            case 24 => Array(0x78, l << 4, 0x6B, 0x20 | r, 0x00, d >> 16, d >> 8, d)
          }
        }
        case (pos: Pos, reg: RegWord) => (visit(pos), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x6D, (l << 4) | r)
        }
        case (abs: AbsAddress, reg: RegWord) => (visit(abs), visit(reg)) match {
          case (Some((a: Int, s: Int)), Some(r: Int)) => s match {
            case 16 => Array(0x6B, r, a >> 8, a)
            case 24 => Array(0x6B, 0x20 | r, 0x00, a >> 16, a >> 8, a)
          }
        }
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, r, l >> 24, l >> 16, l >> 8, l)
        }
        case (lreg: IndirReg, rreg: RegLong) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x01, 0x00, 0x69, (l << 4) | r)
        }
        case (disp: Disp, reg: RegLong) => (visit(disp), visit(reg)) match {
          case (Some((d: Int, l: Int, s: Int)), Some(r: Int)) => s match {
            case 16 => Array(0x01, 0x00, 0x6F, (l << 4) | r, d >> 8, d)
            case 24 => Array(0x01, 0x00, 0x78, l << 4, 0x6B, 0x20 | r, 0x00, d >> 16, d >> 8, d)
          }
        }
        case (pos: Pos, reg: RegLong) => (visit(pos), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x01, 0x00, 0x6D, (l << 4) | r)
        }
        case (abs: AbsAddress, reg: RegLong) => (visit(abs), visit(reg)) match {
          case (Some((a: Int, s: Int)), Some(r: Int)) => s match {
            case 16 => Array(0x01, 0x00, 0x6B, r, a >> 8, a)
            case 24 => Array(0x01, 0x00, 0x6B, 0x20 | r, 0x00, a >> 16, a >> 8, a)
          }
        }
        case (lreg: RegByte, rreg: IndirReg) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x68, 0x80 | (r << 4) | l)
        }
        case (reg: RegByte, disp: Disp) => (visit(reg), visit(disp)) match {
          case (Some(l: Int), Some((d: Int, r: Int, s: Int))) => s match {
            case 16 => Array(0x6E, 0x80 | (r << 4) | l, d >> 8, d)
            case 24 => Array(0x78, r << 4, 0x6A, 0xA0 | l, 0x00, d >> 16, d >> 8, d)
          }
        }
        case (reg: RegByte, pre: Pre) => (visit(reg), visit(pre)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x6C, 0x80 | (r << 4) | l)
        }
        case (reg: RegByte, abs: AbsAddress) => (visit(reg), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, s: Int))) => s match {
            case 8 => Array(0x30 | l, a)
            case 16 => Array(0x6A, 0x80 | l, a >> 8, a)
            case 24 => Array(0x6A, 0xA0 | l, 0x00, a >> 16, a >> 8, a)
          }
        }
        case (lreg: RegWord, rreg: IndirReg) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x69, 0x80 | (r << 4) | l)
        }
        case (reg: RegWord, disp: Disp) =>
          val x = (visit(reg), visit(disp))
          x match {
            case (Some(l: Int), Some((d: Int, r: Int, s: Int))) => s match {
              case 16 => Array(0x6F, 0x80 | (r << 4) | l, d >> 8, d)
              case 24 => Array(0x78, r << 4, 0x6B, 0xA0 | l, 0x00, d >> 16, d >> 8, d)
            }
          }
        case (reg: RegWord, pre: Pre) => (visit(reg), visit(pre)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x6D, 0x80 | (r << 4) | l)
        }
        case (reg: RegWord, abs: AbsAddress) => (visit(reg), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, s: Int))) => s match {
            case 16 => Array(0x6B, 0x80 | l, a >> 8, a)
            case 24 => Array(0x6B, 0xA0 | l, 0x00, a >> 16, a >> 8, a)
          }
        }
        case (lreg: RegLong, rreg: IndirReg) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x01, 0x00, 0x69, 0x80 | (r << 4) | l)
        }
        case (reg: RegLong, disp: Disp) => (visit(reg), visit(disp)) match {
          case (Some(l: Int), Some((d: Int, r: Int, s: Int))) => s match {
            case 16 => Array(0x01, 0x00, 0x6F, 0x80 | (r << 4) | l, d >> 8, d)
            case 24 => Array(0x01, 0x00, 0x78, r << 4, 0x6B, 0xA0 | l, 0x00, d >> 16, d >> 8, d)
          }
        }
        case (reg: RegLong, pre: Pre) => (visit(reg), visit(pre)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x01, 0x00, 0x6D, 0x80 | (r << 4) | l)
        }
        case (reg: RegLong, abs: AbsAddress) => (visit(reg), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, s: Int))) => s match {
            case 16 => Array(0x01, 0x00, 0x6B, 0x80 | l, a >> 8, a)
            case 24 => Array(0x01, 0x00, 0x6B, 0xA0 | l, 0x00, a >> 16, a >> 8, a)
          }
        }
      })

      case Movtpe(left, right) => Some((left, right) match {
        case (reg: RegWord, abs: AbsAddress) => (visit(reg), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x6A, 0x40 | l, a >> 8, a)
        }
      })

      case Movfpe(left, right) => Some((left, right) match {
        case (reg: RegWord, abs: AbsAddress) => (visit(reg), visit(abs)) match {
          case (Some(l: Int), Some((a: Int, _: Int))) => Array(0x6A, 0xC0 | l, a >> 8, a)
        }
      })

      case Mulxs(left, right) => Some((visit(left), visit(right)) match {
        case (Some(l: Int), Some(r: Int)) => Array(0x01, 0x00, (left, right) match {
          case (lreg: RegByte, rreg: RegWord) => 0x50
          case (lreg: RegWord, rreg: RegLong) => 0x52
        }, (l << 4) | r)
      })

      case Mulxu(left, right) => Some((visit(left), visit(right)) match {
        case (Some(l: Int), Some(r: Int)) => Array((left, right) match {
          case (lreg: RegByte, rreg: RegWord) => 0x50
          case (lreg: RegWord, rreg: RegLong) => 0x52
        }, (l << 4) | r)
      })

      case Neg(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x17, r | (reg match {
          case r: RegByte => 0x80
          case r: RegWord => 0x90
          case r: RegLong => 0xB0
        }))
      })

      case Nop() => Some(Array(0x00, 0x00))

      case Not(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x17, r | (reg match {
          case r: RegByte => 0x00
          case r: RegWord => 0x10
          case r: RegLong => 0x30
        }))
      })

      case Or(left, right) => Some((left, right) match {
        case (imm: Imm, rreg: RegByte) => (visit(imm), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0xC0 | r, l)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x14, (l << 4) | r)
        }
        case (imm: Imm, rreg: RegWord) => (visit(imm), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, 0x40 | r, l >> 8, l)
        }
        case (lreg: RegWord, rreg: RegWord) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x64, (l << 4) | r)
        }
        case (imm: Imm, rreg: RegLong) => (visit(imm), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, 0x40 | r, l >> 24, l >> 16, l >> 8, l)
        }
        case (lreg: RegLong, rreg: RegLong) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x01, 0xF0, 0x64, (l << 4) | r)
        }
      })

      case Orc(imm) => Some(imm match {
        case i: Imm => visit(i) match {
          case Some(n: Int) => Array(0x04, n)
        }
      })

      case Pop(reg) => Some(reg match {
        case (r: RegWord) => visit(r) match {
          case Some(n: Int) => Array(0x6D, 0x70 | n)
        }
        case (r: RegLong) => visit(r) match {
          case Some(n: Int) => Array(0x01, 0x00, 0x6D, 0x70 | n)
        }
      })

      case Push(reg) => Some(reg match {
        case (r: RegWord) => visit(r) match {
          case Some(n: Int) => Array(0x6D, 0xF0 | n)
        }
        case (r: RegLong) => visit(r) match {
          case Some(n: Int) => Array(0x01, 0x00, 0x6D, 0xF0 | n)
        }
      })

      case Rotl(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x12, r | (reg match {
          case r: RegByte => 0x80
          case r: RegWord => 0x90
          case r: RegLong => 0xB0
        }))
      })

      case Rotr(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x13, r | (reg match {
          case r: RegByte => 0x80
          case r: RegWord => 0x90
          case r: RegLong => 0xB0
        }))
      })

      case Rotxl(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x12, r | (reg match {
          case r: RegByte => 0x00
          case r: RegWord => 0x10
          case r: RegLong => 0x30
        }))
      })

      case Rotxr(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x13, r | (reg match {
          case r: RegByte => 0x00
          case r: RegWord => 0x10
          case r: RegLong => 0x30
        }))
      })

      case Rte() => Some(Array(0x56, 0x70))

      case Rts() => Some(Array(0x54, 0x70))

      case Shal(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x10, r | (reg match {
          case r: RegByte => 0x80
          case r: RegWord => 0x90
          case r: RegLong => 0xB0
        }))
      })

      case Shar(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x11, r | (reg match {
          case r: RegByte => 0x80
          case r: RegWord => 0x90
          case r: RegLong => 0xB0
        }))
      })

      case Shll(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x10, r | (reg match {
          case r: RegByte => 0x00
          case r: RegWord => 0x10
          case r: RegLong => 0x30
        }))
      })

      case Shlr(reg) => Some(visit(reg) match {
        case Some(r: Int) => Array(0x11, r | (reg match {
          case r: RegByte => 0x00
          case r: RegWord => 0x10
          case r: RegLong => 0x30
        }))
      })

      case Sleep() => Some(Array(0x01, 0x80))

      case Stc(item) => Some(item match {
        case reg: RegByte => visit(reg) match {
          case Some(r: Int) => Array(0x02, r)
        }
        case reg: IndirReg => visit(reg) match {
          case Some(r: Int) => Array(0x01, 0x40, 0x69, 0x80 | (r << 4))
        }
        case disp: Disp => visit(disp) match {
          case Some((d: Int, r: Int, s: Int)) => s match {
            case 16 => Array(0x01, 0x40, 0x6F, 0x80 | (r << 4), d >> 8, d)
            case 32 => Array(0x01, 0x40, 0x78, r << 4, 0x6B, 0xA0, 0x00, d >> 16, d >> 8, d)
          }
        }
        case pre: Pre => visit(pre) match {
          case Some(r: Int) => Array(0x01, 0x40, 0x6D, 0x80 | (r << 4))
        }
        case abs: AbsAddress => visit(abs) match {
          case Some((a: Int, s: Int)) => s match {
            case 16 => Array(0x01, 0x40, 0x6B, 0x80, a >> 8, a)
            case 32 => Array(0x01, 0x40, 0x6B, 0xA0, 0x00, a >> 16, a >> 8, a)
          }
        }
      })

      case Sub(left, right) => Some((left, right) match {
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x18, (l << 4) | r)
        }
        case (imm: Imm, reg: RegWord) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, 0x30 | r, l >> 8, l)
        }
        case (lreg: RegWord, rreg: RegWord) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x19, (l << 4) | r)
        }
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, 0x30 | r, l >> 24, l >> 16, l >> 8, l)
        }
        case (lreg: RegLong, rreg: RegLong) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1A, 0x80 | (l << 4) | r)
        }
      })

      case Subs(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1B, r | (l match {
            case 1 => 0x00
            case 2 => 0x80
            case 4 => 0x90
          }))
        }
      })

      case Subx(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0xB0 | r, l)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1E, (l << 4) | r)
        }
      })

      case Trapa(imm) => Some(imm match {
        case i: Imm => visit(i) match {
          case Some(n: Int) => Array(0x57, n << 4)
        }
      })

      case Xor(left, right) => Some((left, right) match {
        case (imm: Imm, reg: RegByte) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0xD0 | r, l)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x15, (l << 4) | r)
        }
        case (imm: Imm, reg: RegWord) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, 0x50 | r, l >> 8, l)
        }
        case (lreg: RegWord, rreg: RegWord) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x65, (l << 4) | r)
        }
        case (imm: Imm, reg: RegLong) => (visit(imm), visit(reg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, 0x50 | r, l >> 24, l >> 16, l >> 8, l)
        }
        case (lreg: RegLong, rreg: RegLong) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x01, 0xF0, 0x65, (l << 4) | r)
        }
      })

      case Xorc(imm) => Some(imm match {
        case i: Imm => visit(i) match {
          case Some(n: Int) => Array(0x05, n)
        }
      })

      case Data(num, size) => Some(visit(num) match {
        case Some(n: Int) => size match {
          case 8 => Array(n)
          case 16 => Array(n >> 8, n)
          case 32 => Array(n >> 24, n >> 16, n >> 8, n)
        }
      })

      case DataBlock(block, data, size) => Some((visit(block), visit(data)) match {
        case (Some(b: Int), Some(d: Int)) => size match {
          case 8 => Array.fill(b)(d)
          case 16 =>
            val array = new Array[Int](2 * b)
            for (i <- 0 until array.length)
              array(i) =
                if (i % 2 == 0) d >> 8
                else d
            array
          case 32 =>
            val array = new Array[Int](4 * b)
            for (i <- 0 until array.length)
              array(i) =
                if (i % 4 == 0) d >> 24
                else if (i % 4 == 1) d >> 16
                else if (i % 4 == 2) d >> 8
                else d
            array
        }
      })

      case LabelName(num) => Some(label(num))

      case MakeLabel(num) => None

      case Imm(num) => Some(visit(num) match {
        case Some(n: Int) => n
      })

      case AbsAddress(num, size) => Some(visit(num) match {
        case Some(n: Int) => (n, size)
      })

      case IndirReg(reg) => Some(visit(reg) match {
        case Some(r: Int) => r
      })

      case IndirAdd(add) => Some(visit(add) match {
        case Some(a: Int) => a
      })

      case Disp(disp, reg, size) => Some((visit(disp), visit(reg)) match {
        case (Some(d: Int), Some(r: Int)) => (d, r, size)
      })

      case Pos(reg) => Some(visit(reg) match {
        case Some(r: Int) => r
      })

      case Pre(reg) => Some(visit(reg) match {
        case Some(r: Int) => r
      })

      case Section(sec) =>
        section = sec
        None

      case RegByte(num) => Some(num)

      case RegWord(num) => Some(num)

      case RegLong(num) => Some(num)

      case Number(num) => Some(num)

      case Minus(num) => Some(visit(num) match {
        case Some(n: Int) => -n
      })

      case Rev(num) => Some(visit(num) match {
        case Some(n: Int) => ~n
      })

      case High(num) => Some(visit(num) match {
        case Some(n: Int) => (n >> 8) & 0xFF
      })

      case Low(num) => Some(visit(num) match {
        case Some(n: Int) => n & 0xFF
      })

      case HWord(num) => Some(visit(num) match {
        case Some(n: Int) => (n >> 16) & 0xFFFF
      })

      case LWord(num) => Some(visit(num) match {
        case Some(n: Int) => n & 0xFFFF
      })

      case StartOf(sec) => Some(Parameter.start(sec))

      case SizeOf(sec) => Some(count(sec))

      case Expr(op, left, right) => Some((visit(left), visit(right)) match {
        case (Some(l: Int), Some(r: Int)) => op match {
          case "+" => l + r
          case "-" => l - r
          case "*" => l * r
          case "/" => l / r
          case "&" => l & r
          case "|" => l | r
          case "~" => l ^ r
          case ">>" => l >> r
          case "<<" => l << r
        }
      })
    }
  }

  class MyHashMap extends mutable.HashMap[String, Int] {

    override def put(key: String, value: Int): Option[Int] = {
      key match {
        case "V" =>
          super.put("V", countup("V", value))
        case "P" =>
          super.put("P", countup("P", value))
          super.put("C", countup("C", value))
          super.put("D", countup("D", value))
        case "C" =>
          super.put("C", countup("C", value))
          super.put("D", countup("D", value))
        case "D" =>
          super.put("D", countup("D", value))
        case "B" =>
          super.put("B", countup("B", value))
          super.put("R", countup("R", value))
        case "R" =>
          super.put("R", countup("R", value))
      }
    }

    def countup(key: String, value: Int): Int = super.get(key).get + value
  }

}