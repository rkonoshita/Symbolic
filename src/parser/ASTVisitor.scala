package parser

import java.io.File

import data.register.ROM
import main.Parameter
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
      parseResult += new ASTParser().parse(l).get
    }}

    //意味解析１回目：ラベルの位置を決める
    parseResult.foreach { p =>
      //count.putの中にnumの中身を直接入れたらダメ
      //numを介して代入すること
      search(p) match {
        case v: VisitInt => count.put(section, v.item)
        case _ =>
      }
    }

    val rom = new mutable.HashMap[Int, Byte]
    //意味解析してメモリ上にデータを配置していく
    parseResult.foreach { p =>
      println(p)
      visit(p) match {
        case vi: VisitArray =>
          (0 until vi.item.length).foreach { op =>
            print("%x".format(vi.item(op).toByte) + ":")
            rom += (tmppc(section) + op) -> vi.item(op).toByte
          }
          tmppc.put(section, vi.item.length)
        case _ =>
      }
      println
    }
    Parameter.sizeset(count)
    new ROM(rom)
  }

  //ラベル位置の捜索
  //文法的な正しさは度外視
  def search(ast: AST): Visit = {
    ast match {
      case Add(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => new VisitInt(4)
        case (_: Imm, _: RegLong) => new VisitInt(6)
        case _ => new VisitInt(2)
      }

      case AddSign(left, right) => new VisitInt(2)

      case AddExtends(left, right) => new VisitInt(2)

      case And(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => new VisitInt(4)
        case (_: Imm, _: RegLong) => new VisitInt(6)
        case (_: RegLong, _: RegLong) => new VisitInt(4)
        case _ => new VisitInt(2)
      }

      case Andc(item) => new VisitInt(2)

      case Band(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bra(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Brn(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bhi(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bls(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bcc(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bcs(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bne(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Beq(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bvc(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bvs(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bpl(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bmi(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bge(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Blt(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bgt(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Ble(num, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bclr(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case (_: RegByte, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Biand(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bild(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bior(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bist(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bixor(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bld(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bnot(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bor(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bset(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case (_: RegByte, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bsr(disp, size) => new VisitInt(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bst(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Btst(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case (_: RegByte, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Bxor(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => new VisitInt(2)
        case _ => new VisitInt(4)
      }

      case Cmp(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => new VisitInt(4)
        case (_: Imm, _: RegLong) => new VisitInt(6)
        case _ => new VisitInt(2)
      }

      case Daa(reg) => new VisitInt(2)

      case Das(reg) => new VisitInt(2)

      case Dec(left, right) => new VisitInt(2)

      case Divxs(left, right) => new VisitInt(4)

      case Divxu(left, right) => new VisitInt(2)

      case Eepmov(size) => new VisitInt(4)

      case Exts(reg) => new VisitInt(2)

      case Extu(reg) => new VisitInt(2)

      case Inc(left, right) => new VisitInt(2)

      case Jmp(add) => add match {
        case a: AbsAddress => new VisitInt(4)
        case _ => new VisitInt(2)
      }

      case Jsr(add) => add match {
        case a: AbsAddress => new VisitInt(4)
        case _ => new VisitInt(2)
      }

      case Ldc(reg) => reg match {
        case _: IndirReg => new VisitInt(4)
        case r: Disp => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 16 => 6
            case 24 => 10
          }
        })
        case _: Pos => new VisitInt(4)
        case r: AbsAddress => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 8 => 2
            case 16 => 6
            case 24 => 10
          }
        })
        case _ => new VisitInt(2)
      }

      case Mov(left, right) => (left, right) match {
        case (l: Disp, _: RegByte) => new VisitInt(search(l) match {
          case s: VisitInt => s.item match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (l: AbsAddress, _: RegByte) => new VisitInt(search(l) match {
          case s: VisitInt => s.item match {
            case 8 => 2
            case 16 => 4
            case 24 => 6
          }
        })
        case (_: Imm, _: RegWord) => new VisitInt(4)
        case (l: Disp, _: RegWord) => new VisitInt(search(l) match {
          case s: VisitInt => s.item match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (l: AbsAddress, _: RegWord) => new VisitInt(search(l) match {
          case s: VisitInt => s.item match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (_: Imm, _: RegLong) => new VisitInt(6)
        case (_: IndirReg, _: RegLong) => new VisitInt(4)
        case (l: Disp, _: RegLong) => new VisitInt(search(l) match {
          case s: VisitInt => s.item match {
            case 16 => 6
            case 24 => 10
          }
        })
        case (_: Pos, _: RegLong) => new VisitInt(4)
        case (l: AbsAddress, _: RegLong) => new VisitInt(search(l) match {
          case s: VisitInt => s.item match {
            case 16 => 6
            case 24 => 8
          }
        })
        case (_: RegByte, r: Disp) => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (_: RegByte, r: AbsAddress) => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 8 => 2
            case 16 => 4
            case 24 => 6
          }
        })
        case (_: RegWord, r: Disp) => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (_: RegWord, r: AbsAddress) => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 16 => 4
            case 24 => 6
          }
        })
        case (_: RegLong, _: IndirReg) => new VisitInt(4)
        case (_: RegLong, r: Disp) => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 16 => 6
            case 24 => 10
          }
        })
        case (_: RegLong, _: Pre) => new VisitInt(4)
        case (_: RegLong, r: AbsAddress) => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 16 => 4
            case 24 => 6
          }
        })
        case _ => new VisitInt(2)
      }

      case Movfpe(lefr, right) => new VisitInt(4)

      case Movtpe(left, right) => new VisitInt(4)

      case Mulxs(left, right) => new VisitInt(4)

      case Mulxu(left, right) => new VisitInt(2)

      case Neg(reg) => new VisitInt(2)

      case Nop() => new VisitInt(2)

      case Not(item) => new VisitInt(2)

      case Or(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => new VisitInt(4)
        case (_: Imm, _: RegLong) => new VisitInt(6)
        case (_: RegLong, _: RegLong) => new VisitInt(4)
        case _ => new VisitInt(2)
      }

      case Orc(item) => new VisitInt(2)

      case Pop(reg) => reg match {
        case (_: RegWord) => new VisitInt(2)
        case (_: RegLong) => new VisitInt(4)
      }

      case Push(reg) => reg match {
        case (_: RegWord) => new VisitInt(2)
        case (_: RegLong) => new VisitInt(4)
      }

      case Rotl(reg) => new VisitInt(2)

      case Rotr(reg) => new VisitInt(2)

      case Rotxl(reg) => new VisitInt(2)

      case Rotxr(reg) => new VisitInt(2)

      case Rte() => new VisitInt(2)

      case Rts() => new VisitInt(2)

      case Shal(reg) => new VisitInt(2)

      case Shar(reg) => new VisitInt(2)

      case Shll(reg) => new VisitInt(2)

      case Shlr(reg) => new VisitInt(2)

      case Sleep() => new VisitInt(2)

      case Stc(reg) => reg match {
        case _: RegByte => new VisitInt(2)
        case r: Disp => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 16 => 6
            case 24 => 10
          }
        })
        case r: AbsAddress => new VisitInt(search(r) match {
          case s: VisitInt => s.item match {
            case 8 => 2
            case 16 => 6
            case 24 => 8
          }
        })
        case _ => new VisitInt(4)
      }

      case Sub(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => new VisitInt(4)
        case (_: Imm, _: RegLong) => new VisitInt(6)
        case _ => new VisitInt(2)
      }

      case Subs(left, right) => new VisitInt(2)

      case Subx(left, right) => new VisitInt(2)

      case Trapa(imm) => new VisitInt(2)

      case Xor(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => new VisitInt(4)
        case (_: Imm, _: RegLong) => new VisitInt(6)
        case (_: RegLong, _: RegLong) => new VisitInt(4)
        case _ => new VisitInt(2)
      }

      case Xorc(imm) => new VisitInt(2)

      case Data(num, size) => size match {
        case 8 => new VisitInt(1)
        case 16 => new VisitInt(2)
        case 32 => new VisitInt(4)
      }

      case DataBlock(block, data, size) => size match {
        case 8 => new VisitInt(1 * (search(block) match {
          case i: VisitInt => i.item
        }))
        case 16 => new VisitInt(2 * (search(block) match {
          case i: VisitInt => i.item
        }))
        case 32 => new VisitInt(4 * (search(block) match {
          case i: VisitInt => i.item
        }))
      }

      case AbsAddress(num, size) => new VisitInt(size)

      case Disp(disp, reg, size) => new VisitInt(size)

      case LabelName(str) => new VisitString(str)

      case Number(num) => new VisitInt(num)

      case MakeLabel(name) => label += (search(name) match {
        case s: VisitString => s.item -> count(section)
      })
        new VisitString("") //ダミー

      case Section(sec) =>
        section = sec
        new VisitString("") //ダミー
    }
  }

  //メモリに命令やデータを配置
  def visit(ast: AST): Visit = {
    ast match {
      case Add(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x80 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: RegByte) => new VisitArray(0x08, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x79, 0x10 | visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: RegWord, r: RegWord) => new VisitArray(0x09, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x7A, 0x10 | visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: RegLong, r: RegLong) => new VisitArray(0x0A, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case AddSign(left, right) => (left, right) match {
        case (l: Imm, r: RegLong) => new VisitArray(0x0B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x00
          case 2 => 0x80
          case 4 => 0x90
        }))
      }

      case AddExtends(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x90 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: RegByte) => new VisitArray(0x0E, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case And(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0xE0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: RegByte) => new VisitArray(0x16, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x79, 0x60 | visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: RegWord, r: RegWord) => new VisitArray(0x66, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x7A, 0x60 | visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: RegLong, r: RegLong) => new VisitArray(0x01, 0xF0, 0x66, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Andc(item) => new VisitArray(0x06, visit(item).asInstanceOf[VisitInt].item)

      case Band(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x76, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x76, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x76, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bra(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x40, disp)
          case 16 => new VisitArray(0x58, 0x00, disp >> 8, disp)
        }

      case Brn(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x41, disp)
          case 16 => new VisitArray(0x58, 0x10, disp >> 8, disp)
        }

      case Bhi(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x42, disp)
          case 16 => new VisitArray(0x58, 0x20, disp >> 8, disp)
        }

      case Bls(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x43, disp)
          case 16 => new VisitArray(0x58, 0x30, disp >> 8, disp)
        }

      case Bcc(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x44, disp)
          case 16 => new VisitArray(0x58, 0x40, disp >> 8, disp)
        }

      case Bcs(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x45, disp)
          case 16 => new VisitArray(0x58, 0x50, disp >> 8, disp)
        }

      case Bne(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x46, disp)
          case 16 => new VisitArray(0x58, 0x60, disp >> 8, disp)
        }

      case Beq(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x47, disp)
          case 16 => new VisitArray(0x58, 0x70, disp >> 8, disp)
        }

      case Bvc(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x48, disp)
          case 16 => new VisitArray(0x58, 0x80, disp >> 8, disp)
        }

      case Bvs(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x49, disp)
          case 16 => new VisitArray(0x58, 0x90, disp >> 8, disp)
        }

      case Bpl(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x4A, disp)
          case 16 => new VisitArray(0x58, 0xA0, disp >> 8, disp)
        }

      case Bmi(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x4B, disp)
          case 16 => new VisitArray(0x58, 0xB0, disp >> 8, disp)
        }

      case Bge(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x4C, disp)
          case 16 => new VisitArray(0x58, 0xC0, disp >> 8, disp)
        }

      case Blt(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x4D, disp)
          case 16 => new VisitArray(0x58, 0xD0, disp >> 8, disp)
        }

      case Bgt(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x4E, disp)
          case 16 => new VisitArray(0x58, 0xE0, disp >> 8, disp)
        }

      case Ble(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x4F, disp)
          case 16 => new VisitArray(0x58, 0xF0, disp >> 8, disp)
        }

      case Bclr(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x72, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x72, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x72, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: RegByte) => new VisitArray(0x62, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x62, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x62, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Biand(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x76, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x76, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x76, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bild(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x77, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x77, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x77, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bior(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x74, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x74, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x74, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bist(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x67, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x67, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x67, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bixor(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x75, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x75, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x75, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bld(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x77, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x77, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x77, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bnot(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x71, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x71, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x71, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: RegByte) => new VisitArray(0x61, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x61, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x61, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bor(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x74, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x74, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x74, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bset(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x70, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x70, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x70, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: RegByte) => new VisitArray(0x60, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x60, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x60, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bsr(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => new VisitArray(0x55, disp)
          case 16 => new VisitArray(0x5C, 0x00, disp >> 8, disp)
        }

      case Bst(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x67, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x67, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x67, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Btst(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x73, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x73, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x73, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: RegByte) => new VisitArray(0x63, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x63, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x63, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bxor(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0x75, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => new VisitArray(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x75, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => new VisitArray(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x75, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Cmp(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0xA0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: RegByte) => new VisitArray(0x1C, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x79, 0x20 | visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: RegWord, r: RegWord) => new VisitArray(0x1D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x7A, 0x20 | visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: RegLong, r: RegLong) => new VisitArray(0x1F, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Daa(reg) => new VisitArray(0x0F, visit(reg).asInstanceOf[VisitInt].item)

      case Das(reg) => new VisitArray(0x1F, visit(reg).asInstanceOf[VisitInt].item)

      case Dec(left, right) => (left, right) match {
        case (l: RegByte, r: Empty) => new VisitArray(0x1A, visit(l).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) => new VisitArray(0x1B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x50
          case 2 => 0xD0
        }))
        case (l: Imm, r: RegLong) => new VisitArray(0x1B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x70
          case 2 => 0xF0
        }))
      }

      case Divxs(left, right) => (left, right) match {
        case (l: RegByte, r: RegWord) => new VisitArray(0x01, 0xD0, 0x51, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegLong) => new VisitArray(0x01, 0xD0, 0x53, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Divxu(left, right) => (left, right) match {
        case (l: RegByte, r: RegWord) => new VisitArray(0x51, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegLong) => new VisitArray(0x53, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Eepmov(size) => new VisitArray(0x7B, size match {
        case 8 => 0x5C
        case 16 => 0xD4
      }, 0x59, 0x8F)

      case Exts(reg) => new VisitArray(0x17, visit(reg).asInstanceOf[VisitInt].item | (reg match {
        case r: RegWord => 0xD0
        case r: RegLong => 0xF0
      }))

      case Extu(reg) => new VisitArray(0x17, visit(reg).asInstanceOf[VisitInt].item | (reg match {
        case r: RegWord => 0x50
        case r: RegLong => 0x70
      }))

      case Inc(left, right) => (left, right) match {
        case (l: RegByte, r: Empty) => new VisitArray(0x0A, visit(l).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) => new VisitArray(0x0B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x50
          case 2 => 0xD0
        }))
        case (l: Imm, r: RegLong) => new VisitArray(0x0B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x70
          case 2 => 0xF0
        }))
      }

      case Jmp(add) => add match {
        case (a: IndirReg) => new VisitArray(0x59, visit(a).asInstanceOf[VisitInt].item << 4)
        case (a: AbsAddress) =>
          val abs = visit(a).asInstanceOf[VisitArray].item
          new VisitArray(0x5A, abs(0) >> 16, abs(0) >> 8, abs(0))
        case (a: IndirAdd) => new VisitArray(0x5B, visit(a).asInstanceOf[VisitInt].item)
      }

      case Jsr(add) => add match {
        case (a: IndirReg) => new VisitArray(0x5D, visit(a).asInstanceOf[VisitInt].item << 4)
        case (a: AbsAddress) =>
          val abs = visit(a).asInstanceOf[VisitArray].item
          new VisitArray(0x5E, abs(0) >> 16, abs(0) >> 8, abs(0))
        case (a: IndirAdd) => new VisitArray(0x5F, visit(a).asInstanceOf[VisitInt].item)
      }

      case Ldc(item) => item match {
        case i: Imm => new VisitArray(0x07, visit(i).asInstanceOf[VisitInt].item)
        case r: RegByte => new VisitArray(0x03, visit(r).asInstanceOf[VisitInt].item)
        case r: IndirReg => new VisitArray(0x01, 0x40, 0x69, visit(r).asInstanceOf[VisitInt].item << 4)
        case d: Disp =>
          val disp = visit(d).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => new VisitArray(0x01, 0x40, 0x6F, disp(1) << 4, disp(0) << 8, disp(0))
            case 24 => new VisitArray(0x01, 0x40, 0x78, disp(1) << 4, 0x6B, 0x20, 0x00, disp(0) << 16, disp(0) << 8, disp(0))
          }
        case p: Pos => new VisitArray(0x01, 0x40, 0x6D, visit(p).asInstanceOf[VisitInt].item << 4)
        case a: AbsAddress =>
          val abs = visit(a).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => new VisitArray(0x01, 0x40, 0x6B, 0x00, abs(0) << 8, abs(0))
            case 24 => new VisitArray(0x01, 0x40, 0x6B, 0x20, 0x00, abs(0) << 16, abs(0) << 8, abs(0))
          }
      }

      case Mov(left, right) => (left, right) match {
        case (l: RegByte, r: RegByte) => new VisitArray(0x0C, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegWord) => new VisitArray(0x0D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegLong, r: RegLong) => new VisitArray(0x0F, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegByte) => new VisitArray(0xF0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: IndirReg, r: RegByte) => new VisitArray(0x68, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Disp, r: RegByte) =>
          val disp = visit(l).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => new VisitArray(0x6E, (disp(1) << 4) | visit(r).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => new VisitArray(0x78, disp(1) << 4, 0x6A, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: Pos, r: RegByte) => new VisitArray(0x6C, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: AbsAddress, r: RegByte) =>
          val abs = visit(l).asInstanceOf[VisitArray].item
          abs(1) match {
            case 8 => new VisitArray(0x20 | visit(r).asInstanceOf[VisitInt].item, abs(0))
            case 16 => new VisitArray(0x6A, visit(r).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => new VisitArray(0x6A, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x79, visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: IndirReg, r: RegWord) => new VisitArray(0x69, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Disp, r: RegWord) =>
          val disp = visit(l).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => new VisitArray(0x6F, (disp(1) << 4) | visit(r).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => new VisitArray(0x78, disp(1) << 4, 0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: Pos, r: RegWord) => new VisitArray(0x6D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: AbsAddress, r: RegWord) =>
          val abs = visit(l).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => new VisitArray(0x6B, visit(r).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => new VisitArray(0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x7A, visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: IndirReg, r: RegLong) => new VisitArray(0x01, 0x00, 0x69, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Disp, r: RegLong) =>
          val disp = visit(l).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => new VisitArray(0x01, 0x00, 0x6F, (disp(1) << 4) | visit(r).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => new VisitArray(0x01, 0x00, 0x78, disp(1) << 4, 0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: Pos, r: RegLong) => new VisitArray(0x01, 0x00, 0x6D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: AbsAddress, r: RegLong) =>
          val abs = visit(l).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => new VisitArray(0x01, 0x00, 0x6B, visit(r).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => new VisitArray(0x01, 0x00, 0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: RegByte, r: IndirReg) => new VisitArray(0x68, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: Disp) =>
          val disp = visit(r).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => new VisitArray(0x6E, 0x80 | (disp(1) << 4) | visit(l).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => new VisitArray(0x78, disp(1) << 4, 0x6A, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: RegByte, r: Pre) => new VisitArray(0x6C, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: AbsAddress) =>
          val abs = visit(r).asInstanceOf[VisitArray].item
          abs(1) match {
            case 8 => new VisitArray(0x30 | visit(l).asInstanceOf[VisitInt].item, abs(0))
            case 16 => new VisitArray(0x6A, 0x80 | visit(l).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => new VisitArray(0x6A, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: RegWord, r: IndirReg) => new VisitArray(0x69, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: Disp) =>
          val disp = visit(r).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => new VisitArray(0x6F, 0x80 | (disp(1) << 4) | visit(l).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => new VisitArray(0x78, disp(1) << 4, 0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: RegWord, r: Pre) => new VisitArray(0x6D, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: AbsAddress) =>
          val abs = visit(r).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => new VisitArray(0x6B, 0x80 | visit(l).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => new VisitArray(0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: RegLong, r: IndirReg) => new VisitArray(0x01, 0x00, 0x69, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegLong, r: Disp) =>
          val disp = visit(r).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => new VisitArray(0x01, 0x00, 0x6F, 0x80 | (disp(1) << 4) | visit(l).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => new VisitArray(0x01, 0x00, 0x78, 0x80 | (disp(1) << 4), 0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: RegLong, r: Pre) => new VisitArray(0x01, 0x00, 0x6D, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegLong, r: AbsAddress) =>
          val abs = visit(r).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => new VisitArray(0x01, 0x00, 0x6B, 0x80 | visit(l).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => new VisitArray(0x01, 0x00, 0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
      }

      case Movfpe(left, right) => (left, right) match {
        case (l: AbsAddress, r: RegWord) =>
          val abs = visit(l).asInstanceOf[VisitArray].item(0)
          new VisitArray(0x6A, 0x40 | visit(r).asInstanceOf[VisitInt].item, abs << 8, abs)
      }

      case Movtpe(left, right) => (left, right) match {
        case (l: RegWord, r: AbsAddress) =>
          val abs = visit(r).asInstanceOf[VisitArray].item(0)
          new VisitArray(0x6A, 0xC0 | visit(l).asInstanceOf[VisitInt].item, abs << 8, abs)
      }

      case Mulxs(left, right) => (left, right) match {
        case (l: RegByte, r: RegWord) => new VisitArray(0x01, 0xC0, 0x50, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegLong) => new VisitArray(0x01, 0xC0, 0x52, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Mulxu(left, right) => (left, right) match {
        case (l: RegByte, r: RegWord) => new VisitArray(0x50, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegLong) => new VisitArray(0x52, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Neg(reg) => reg match {
        case r: RegByte => new VisitArray(0x17, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x17, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x17, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Nop() => new VisitArray(0x00, 0x00)

      case Not(reg) => reg match {
        case r: RegByte => new VisitArray(0x17, visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x17, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x17, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Or(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0xC0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: RegByte) => new VisitArray(0x14, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x79, 0x40 | visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: RegWord, r: RegWord) => new VisitArray(0x64, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x7A, 0x40 | visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: RegLong, r: RegLong) => new VisitArray(0x01, 0xF0, 0x64, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Orc(imm) => imm match {
        case i: Imm => new VisitArray(0x04, visit(imm).asInstanceOf[VisitInt].item)
      }

      case Pop(reg) => reg match {
        case (r: RegWord) => new VisitArray(0x6D, 0x70 | visit(r).asInstanceOf[VisitInt].item)
        case (r: RegLong) => new VisitArray(0x01, 0x00, 0x6D, 0x70 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Push(reg) => reg match {
        case (r: RegWord) => new VisitArray(0x6D, 0xF0 | visit(r).asInstanceOf[VisitInt].item)
        case (r: RegLong) => new VisitArray(0x01, 0x00, 0x6D, 0xF0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rotl(reg) => reg match {
        case r: RegByte => new VisitArray(0x12, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x12, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x12, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rotr(reg) => reg match {
        case r: RegByte => new VisitArray(0x13, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x13, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x13, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rotxl(reg) => reg match {
        case r: RegByte => new VisitArray(0x12, 0x00 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x12, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x12, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rotxr(reg) => reg match {
        case r: RegByte => new VisitArray(0x13, 0x00 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x13, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x13, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rte() => new VisitArray(0x56, 0x70)

      case Rts() => new VisitArray(0x54, 0x70)

      case Shal(reg) => reg match {
        case r: RegByte => new VisitArray(0x10, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x10, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x10, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Shar(reg) => reg match {
        case r: RegByte => new VisitArray(0x11, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x11, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x11, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Shll(reg) => reg match {
        case r: RegByte => new VisitArray(0x10, 0x00 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x10, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x10, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Shlr(reg) => reg match {
        case r: RegByte => new VisitArray(0x11, 0x00 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => new VisitArray(0x11, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => new VisitArray(0x11, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Sleep() => new VisitArray(0x01, 0x80)

      case Stc(item) => item match {
        case r: RegByte => new VisitArray(0x02, visit(r).asInstanceOf[VisitInt].item)
        case r: IndirReg => new VisitArray(0x01, 0x40, 0x69, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4))
        case d: Disp =>
          val disp = visit(d).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => new VisitArray(0x01, 0x40, 0x6F, 0x80 | (disp(1) << 4), disp(0) << 8, disp(0))
            case 24 => new VisitArray(0x01, 0x40, 0x78, disp(1) << 4, 0x6B, 0xA0, 0x00, disp(0) << 16, disp(0) << 8, disp(0))
          }
        case p: Pre => new VisitArray(0x01, 0x40, 0x6D, 0x80 | (visit(p).asInstanceOf[VisitInt].item << 4))
        case a: AbsAddress =>
          val abs = visit(a).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => new VisitArray(0x01, 0x40, 0x6B, 0x80, abs(0) << 8, abs(0))
            case 24 => new VisitArray(0x01, 0x40, 0x6B, 0xA0, 0x00, abs(0) << 16, abs(0) << 8, abs(0))
          }
      }

      case Sub(left, right) => (left, right) match {
        case (l: RegByte, r: RegByte) => new VisitArray(0x18, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x79, 0x30 | visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: RegWord, r: RegWord) => new VisitArray(0x19, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x7A, 0x30 | visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: RegLong, r: RegLong) => new VisitArray(0x1A, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Subs(left, right) => (left, right) match {
        case (l: Imm, r: RegLong) => new VisitArray(0x1B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x00
          case 2 => 0x80
          case 4 => 0x90
        }))
      }

      case Subx(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0xB0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: RegByte) => new VisitArray(0x1E, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Trapa(imm) => imm match {
        case i: Imm => new VisitArray(0x57, visit(i).asInstanceOf[VisitInt].item << 4)
      }

      case Xor(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => new VisitArray(0xD0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: RegByte) => new VisitArray(0x15, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x79, 0x50 | visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: RegWord, r: RegWord) => new VisitArray(0x65, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          new VisitArray(0x7A, 0x50 | visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: RegLong, r: RegLong) => new VisitArray(0x01, 0xF0, 0x65, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Xorc(imm) => imm match {
        case i: Imm => new VisitArray(0x05, visit(imm).asInstanceOf[VisitInt].item)
      }

      case Data(num, size) => size match {
        case 8 => new VisitArray(visit(num).asInstanceOf[VisitInt].item)
        case 16 =>
          val data = visit(num).asInstanceOf[VisitInt].item
          new VisitArray(data >> 8, data)
        case 32 =>
          val data = visit(num).asInstanceOf[VisitInt].item
          new VisitArray(data >> 24, data >> 16, data >> 8, data)
      }

      case DataBlock(block, data, size) => size match {
        case 8 => new VisitArray(Array.fill(visit(block).asInstanceOf[VisitInt].item)(visit(data).asInstanceOf[VisitInt].item))
        case 16 =>
          val d = visit(data).asInstanceOf[VisitInt].item
          val array = new Array[Int](2 * visit(block).asInstanceOf[VisitInt].item)
          for (i <- 0 until array.length / 2) {
            array(i * 2) = (d >> 8)
            array(i * 2 + 1) = d
          }
          new VisitArray(array)
        case 32 =>
          val d = visit(data).asInstanceOf[VisitInt].item
          val array = new Array[Int](4 * visit(block).asInstanceOf[VisitInt].item)
          for (i <- 0 until array.length / 4) {
            array(i * 4) = d >> 24
            array(i * 4 + 1) = d >> 16
            array(i * 4 + 2) = d >> 8
            array(i * 4 + 3) = d
          }
          new VisitArray(array)
      }

      case LabelName(num) => new VisitInt(label(num))

      case MakeLabel(num) => new VisitInt(0)

      case Imm(num) => visit(num)

      case AbsAddress(num, size) => new VisitArray(visit(num).asInstanceOf[VisitInt].item, size)

      case IndirReg(reg) => visit(reg)

      case IndirAdd(add) => visit(add)

      case Disp(disp, reg, size) => new VisitArray(visit(disp).asInstanceOf[VisitInt].item, visit(reg).asInstanceOf[VisitInt].item, size)

      case Pos(item) => visit(item)

      case Pre(item) => visit(item)

      case Section(sec) =>
        section = sec
        new VisitInt(0) // ダミー

      case RegByte(num) => new VisitInt(num)

      case RegWord(num) => new VisitInt(num)

      case RegLong(num) => new VisitInt(num)

      case Number(num) => new VisitInt(num)

      case Minus(num) => new VisitInt(-visit(num).asInstanceOf[VisitInt].item)

      case Rev(num) => new VisitInt(~visit(num).asInstanceOf[VisitInt].item)

      case High(num) => new VisitInt((visit(num).asInstanceOf[VisitInt].item >> 8) & 0xFF)

      case Low(num) => new VisitInt(visit(num).asInstanceOf[VisitInt].item & 0xFF)

      case HWord(num) => new VisitInt((visit(num).asInstanceOf[VisitInt].item >> 8) & 0xFFFF)

      case LWord(num) => new VisitInt(visit(num).asInstanceOf[VisitInt].item & 0xFFFF)

      case StartOf(sec) => new VisitInt(Parameter.start(sec))

      case SizeOf(sec) => new VisitInt(count(sec))

      case Expr(op, left, right) => op match {
        case "+" => new VisitInt(visit(left).asInstanceOf[VisitInt].item + visit(right).asInstanceOf[VisitInt].item)
        case "-" => new VisitInt(visit(left).asInstanceOf[VisitInt].item - visit(right).asInstanceOf[VisitInt].item)
        case "*" => new VisitInt(visit(left).asInstanceOf[VisitInt].item * visit(right).asInstanceOf[VisitInt].item)
        case "/" => new VisitInt(visit(left).asInstanceOf[VisitInt].item / visit(right).asInstanceOf[VisitInt].item)
        case "&" => new VisitInt(visit(left).asInstanceOf[VisitInt].item & visit(right).asInstanceOf[VisitInt].item)
        case "|" => new VisitInt(visit(left).asInstanceOf[VisitInt].item | visit(right).asInstanceOf[VisitInt].item)
        case "~" => new VisitInt(visit(left).asInstanceOf[VisitInt].item ^ visit(right).asInstanceOf[VisitInt].item)
        case ">>" => new VisitInt(visit(left).asInstanceOf[VisitInt].item >> visit(right).asInstanceOf[VisitInt].item)
        case "<<" => new VisitInt(visit(left).asInstanceOf[VisitInt].item << visit(right).asInstanceOf[VisitInt].item)
      }
    }
  }

}

trait Visit {
  type T
  val item: T
}

class VisitInt(num: Int) extends Visit {
  type T = Int
  val item = num
}

class VisitArray(num: Array[Int]) extends Visit {
  type T = Array[Int]
  val item = num

  def this(num: Int*) = this(num.toArray)
}

class VisitString(num: String) extends Visit {
  type T = String
  val item = num
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

