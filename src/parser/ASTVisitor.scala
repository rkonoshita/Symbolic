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
    }
    }

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
  def search(ast: AST): Option[Any] = {
    ast match {
      case Add(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => Some(4)
        case (_: Imm, _: RegLong) => Some(4)
        case _ => Some(2)
      }

      case AddSign(left, right) => Some(2)

      case AddExtends(left, right) => Some(2)

      case And(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => Some(4)
        case (_: Imm, _: RegLong) => Some(6)
        case (_: RegLong, _: RegLong) => Some(4)
        case _ => Some(2)
      }

      case Andc(item) => Some(2)

      case Band(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

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

      case Bclr(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case (_: RegByte, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Biand(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bild(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bior(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bist(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bixor(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bld(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bnot(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bor(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bset(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case (_: RegByte, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bsr(disp, size) => Some(size match {
        case 8 => 2
        case 16 => 4
      })

      case Bst(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Btst(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case (_: RegByte, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Bxor(left, right) => (left, right) match {
        case (_: Imm, _: RegByte) => Some(2)
        case _ => Some(4)
      }

      case Cmp(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => Some(4)
        case (_: Imm, _: RegLong) => Some(6)
        case _ => Some(2)
      }

      case Daa(reg) => Some(2)

      case Das(reg) => Some(2)

      case Dec(left, right) => Some(2)

      case Divxs(left, right) => Some(4)

      case Divxu(left, right) => Some(2)

      case Eepmov(size) => Some(4)

      case Exts(reg) => Some(2)

      case Extu(reg) => Some(2)

      case Inc(left, right) => Some(2)

      case Jmp(add) => add match {
        case _: AbsAddress => Some(4)
        case _ => Some(2)
      }

      case Jsr(add) => add match {
        case _: AbsAddress => Some(4)
        case _ => Some(2)
      }

      case Ldc(reg) => reg match {
        case _: IndirReg => Some(4)
        case r: Disp => Some(search(r) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 10
          }
        })
        case _: Pos => Some(4)
        case r: AbsAddress => Some(search(r) match {
          case Some(s: Int) => s match {
            case 8 => 2
            case 16 => 6
            case 24 => 10
          }
        })
        case _ => Some(2)
      }

      case Mov(left, right) => (left, right) match {
        case (l: Disp, _: RegByte) => Some(search(l) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (l: AbsAddress, _: RegByte) => Some(search(l) match {
          case Some(s: Int) => s match {
            case 8 => 2
            case 16 => 4
            case 24 => 6
          }
        })
        case (_: Imm, _: RegWord) => Some(4)
        case (l: Disp, _: RegWord) => Some(search(l) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (l: AbsAddress, _: RegWord) => Some(search(l) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (_: Imm, _: RegLong) => Some(6)
        case (_: IndirReg, _: RegLong) => Some(4)
        case (l: Disp, _: RegLong) => Some(search(l) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 10
          }
        })
        case (_: Pos, _: RegLong) => Some(4)
        case (l: AbsAddress, _: RegLong) => Some(search(l) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 8
          }
        })
        case (_: RegByte, r: Disp) => Some(search(r) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (_: RegByte, r: AbsAddress) => Some(search(r) match {
          case Some(s: Int) => s match {
            case 8 => 2
            case 16 => 4
            case 24 => 6
          }
        })
        case (_: RegWord, r: Disp) => Some(search(r) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 8
          }
        })
        case (_: RegWord, r: AbsAddress) => Some(search(r) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 6
          }
        })
        case (_: RegLong, _: IndirReg) => Some(4)
        case (_: RegLong, r: Disp) => Some(search(r) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 10
          }
        })
        case (_: RegLong, _: Pre) => Some(4)
        case (_: RegLong, r: AbsAddress) => Some(search(r) match {
          case Some(s: Int) => s match {
            case 16 => 4
            case 24 => 6
          }
        })
        case _ => Some(2)
      }

      case Movfpe(lefr, right) => Some(4)

      case Movtpe(left, right) => Some(4)

      case Mulxs(left, right) => Some(4)

      case Mulxu(left, right) => Some(2)

      case Neg(reg) => Some(2)

      case Nop() => Some(2)

      case Not(item) => Some(2)

      case Or(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => Some(4)
        case (_: Imm, _: RegLong) => Some(6)
        case (_: RegLong, _: RegLong) => Some(4)
        case _ => Some(2)
      }

      case Orc(item) => Some(2)

      case Pop(reg) => reg match {
        case (_: RegWord) => Some(2)
        case (_: RegLong) => Some(4)
      }

      case Push(reg) => reg match {
        case (_: RegWord) => Some(2)
        case (_: RegLong) => Some(4)
      }

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

      case Stc(reg) => reg match {
        case _: RegByte => Some(2)
        case r: Disp => Some(search(r) match {
          case Some(s: Int) => s match {
            case 16 => 6
            case 24 => 10
          }
        })
        case r: AbsAddress => Some(search(r) match {
          case Some(s: Int) => s match {
            case 8 => 2
            case 16 => 6
            case 24 => 8
          }
        })
        case _ => Some(4)
      }

      case Sub(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => Some(4)
        case (_: Imm, _: RegLong) => Some(6)
        case _ => Some(2)
      }

      case Subs(left, right) => Some(2)

      case Subx(left, right) => Some(2)

      case Trapa(imm) => Some(2)

      case Xor(left, right) => (left, right) match {
        case (_: Imm, _: RegWord) => Some(4)
        case (_: Imm, _: RegLong) => Some(6)
        case (_: RegLong, _: RegLong) => Some(4)
        case _ => Some(2)
      }

      case Xorc(imm) => Some(2)

      case Data(num, size) => size match {
        case 8 => Some(1)
        case 16 => Some(2)
        case 32 => Some(4)
      }

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
        case (imm: Imm, rreg: RegByte) => (visit(imm), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x80 | r, l)
        }
        case (lreg: RegByte, rreg: RegByte) => (visit(lreg), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x08, (l << 4) | r)
        }
        case (imm: Imm, rreg: RegWord) => (visit(imm), visit(rreg)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, 0x10 | r, l >> 8, l)
        }
        case (lr: RegWord, rr: RegWord) => (visit(lr), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x09, (l << 4) | r)
        }
        case (i: Imm, rr: RegLong) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, 0x10 | r, l > 24, l > 16, l >> 8, l)
        }
        case (lr: RegLong, rr: RegLong) => (visit(lr), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0A, 0x80 | (l << 4) | r)
        }
      })

      case AddSign(left, right) => Some((left, right) match {
        case (i: Imm, rr: RegLong) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0B, r | (l match {
            case 1 => 0x00
            case 2 => 0x80
            case 4 => 0x90
          }))
        }
      })

      case AddExtends(left, right) => Some((left, right) match {
        case (i: Imm, rr: RegByte) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x90 | r, l)
        }
        case (lr: RegByte, rr: RegByte) => (visit(lr), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x0E, (l << 4) | r)
        }
      })

      case And(left, right) => Some((left, right) match {
        case (i: Imm, rr: RegByte) => Array(0xE0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (lr: RegByte, rr: RegByte) => Array(0x16, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (i: Imm, rr: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          Array(0x79, 0x60 | visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: RegWord, r: RegWord) => Array(0x66, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          Array(0x7A, 0x60 | visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: RegLong, r: RegLong) => Array(0x01, 0xF0, 0x66, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      })

      case Andc(item) => Array(0x06, visit(item).asInstanceOf[VisitInt].item)

      case Band(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x76, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x76, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x76, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bra(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x40, disp)
          case 16 => Array(0x58, 0x00, disp >> 8, disp)
        }

      case Brn(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x41, disp)
          case 16 => Array(0x58, 0x10, disp >> 8, disp)
        }

      case Bhi(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x42, disp)
          case 16 => Array(0x58, 0x20, disp >> 8, disp)
        }

      case Bls(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x43, disp)
          case 16 => Array(0x58, 0x30, disp >> 8, disp)
        }

      case Bcc(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x44, disp)
          case 16 => Array(0x58, 0x40, disp >> 8, disp)
        }

      case Bcs(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x45, disp)
          case 16 => Array(0x58, 0x50, disp >> 8, disp)
        }

      case Bne(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x46, disp)
          case 16 => Array(0x58, 0x60, disp >> 8, disp)
        }

      case Beq(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x47, disp)
          case 16 => Array(0x58, 0x70, disp >> 8, disp)
        }

      case Bvc(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x48, disp)
          case 16 => Array(0x58, 0x80, disp >> 8, disp)
        }

      case Bvs(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x49, disp)
          case 16 => Array(0x58, 0x90, disp >> 8, disp)
        }

      case Bpl(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x4A, disp)
          case 16 => Array(0x58, 0xA0, disp >> 8, disp)
        }

      case Bmi(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x4B, disp)
          case 16 => Array(0x58, 0xB0, disp >> 8, disp)
        }

      case Bge(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x4C, disp)
          case 16 => Array(0x58, 0xC0, disp >> 8, disp)
        }

      case Blt(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x4D, disp)
          case 16 => Array(0x58, 0xD0, disp >> 8, disp)
        }

      case Bgt(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x4E, disp)
          case 16 => Array(0x58, 0xE0, disp >> 8, disp)
        }

      case Ble(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x4F, disp)
          case 16 => Array(0x58, 0xF0, disp >> 8, disp)
        }

      case Bclr(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x72, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x72, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => Array(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x72, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: RegByte) => Array(0x62, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: IndirReg) => Array(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x62, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: AbsAddress) => Array(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x62, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Biand(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x76, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x76, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x76, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bild(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x77, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x77, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x77, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bior(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x74, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x74, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x74, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bist(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x67, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x67, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => Array(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x67, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bixor(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x75, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x75, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
        case (l: Imm, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x75, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4))
      }

      case Bld(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x77, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x77, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x77, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bnot(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x71, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x71, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => Array(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x71, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: RegByte) => Array(0x61, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: IndirReg) => Array(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x61, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: AbsAddress) => Array(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x61, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bor(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x74, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x74, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x74, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bset(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x70, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x70, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => Array(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x70, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: RegByte) => Array(0x60, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: IndirReg) => Array(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x60, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: AbsAddress) => Array(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x60, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bsr(num, size) =>
        val disp = visit(num).asInstanceOf[VisitInt].item - tmppc("P")
        size match {
          case 8 => Array(0x55, disp)
          case 16 => Array(0x5C, 0x00, disp >> 8, disp)
        }

      case Bst(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x67, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7D, visit(r).asInstanceOf[VisitInt].item << 4, 0x67, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => Array(0x7F, visit(r).asInstanceOf[VisitArray].item(0), 0x67, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Btst(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x73, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x73, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x73, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: RegByte) => Array(0x63, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x63, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: RegByte, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x63, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Bxor(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0x75, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: IndirReg) => Array(0x7C, visit(r).asInstanceOf[VisitInt].item << 4, 0x75, visit(l).asInstanceOf[VisitInt].item << 4)
        case (l: Imm, r: AbsAddress) => Array(0x7E, visit(r).asInstanceOf[VisitArray].item(0), 0x75, visit(l).asInstanceOf[VisitInt].item << 4)
      }

      case Cmp(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0xA0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: RegByte) => Array(0x1C, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          Array(0x79, 0x20 | visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: RegWord, r: RegWord) => Array(0x1D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          Array(0x7A, 0x20 | visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: RegLong, r: RegLong) => Array(0x1F, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Daa(reg) => Array(0x0F, visit(reg).asInstanceOf[VisitInt].item)

      case Das(reg) => Array(0x1F, visit(reg).asInstanceOf[VisitInt].item)

      case Dec(left, right) => (left, right) match {
        case (l: RegByte, r: Empty) => Array(0x1A, visit(l).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) => Array(0x1B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x50
          case 2 => 0xD0
        }))
        case (l: Imm, r: RegLong) => Array(0x1B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x70
          case 2 => 0xF0
        }))
      }

      case Divxs(left, right) => (left, right) match {
        case (l: RegByte, r: RegWord) => Array(0x01, 0xD0, 0x51, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegLong) => Array(0x01, 0xD0, 0x53, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Divxu(left, right) => (left, right) match {
        case (l: RegByte, r: RegWord) => Array(0x51, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegLong) => Array(0x53, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Eepmov(size) => Array(0x7B, size match {
        case 8 => 0x5C
        case 16 => 0xD4
      }, 0x59, 0x8F)

      case Exts(reg) => Array(0x17, visit(reg).asInstanceOf[VisitInt].item | (reg match {
        case r: RegWord => 0xD0
        case r: RegLong => 0xF0
      }))

      case Extu(reg) => Array(0x17, visit(reg).asInstanceOf[VisitInt].item | (reg match {
        case r: RegWord => 0x50
        case r: RegLong => 0x70
      }))

      case Inc(left, right) => (left, right) match {
        case (l: RegByte, r: Empty) => Array(0x0A, visit(l).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) => Array(0x0B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x50
          case 2 => 0xD0
        }))
        case (l: Imm, r: RegLong) => Array(0x0B, visit(r).asInstanceOf[VisitInt].item | (visit(l).asInstanceOf[VisitInt].item match {
          case 1 => 0x70
          case 2 => 0xF0
        }))
      }

      case Jmp(add) => add match {
        case (a: IndirReg) => Array(0x59, visit(a).asInstanceOf[VisitInt].item << 4)
        case (a: AbsAddress) =>
          val abs = visit(a).asInstanceOf[VisitArray].item
          Array(0x5A, abs(0) >> 16, abs(0) >> 8, abs(0))
        case (a: IndirAdd) => Array(0x5B, visit(a).asInstanceOf[VisitInt].item)
      }

      case Jsr(add) => add match {
        case (a: IndirReg) => Array(0x5D, visit(a).asInstanceOf[VisitInt].item << 4)
        case (a: AbsAddress) =>
          val abs = visit(a).asInstanceOf[VisitArray].item
          Array(0x5E, abs(0) >> 16, abs(0) >> 8, abs(0))
        case (a: IndirAdd) => Array(0x5F, visit(a).asInstanceOf[VisitInt].item)
      }

      case Ldc(item) => item match {
        case i: Imm => Array(0x07, visit(i).asInstanceOf[VisitInt].item)
        case r: RegByte => Array(0x03, visit(r).asInstanceOf[VisitInt].item)
        case r: IndirReg => Array(0x01, 0x40, 0x69, visit(r).asInstanceOf[VisitInt].item << 4)
        case d: Disp =>
          val disp = visit(d).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => Array(0x01, 0x40, 0x6F, disp(1) << 4, disp(0) << 8, disp(0))
            case 24 => Array(0x01, 0x40, 0x78, disp(1) << 4, 0x6B, 0x20, 0x00, disp(0) << 16, disp(0) << 8, disp(0))
          }
        case p: Pos => Array(0x01, 0x40, 0x6D, visit(p).asInstanceOf[VisitInt].item << 4)
        case a: AbsAddress =>
          val abs = visit(a).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => Array(0x01, 0x40, 0x6B, 0x00, abs(0) << 8, abs(0))
            case 24 => Array(0x01, 0x40, 0x6B, 0x20, 0x00, abs(0) << 16, abs(0) << 8, abs(0))
          }
      }

      case Mov(left, right) => (left, right) match {
        case (l: RegByte, r: RegByte) => Array(0x0C, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegWord) => Array(0x0D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegLong, r: RegLong) => Array(0x0F, 0x80 | (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegByte) => Array(0xF0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: IndirReg, r: RegByte) => Array(0x68, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Disp, r: RegByte) =>
          val disp = visit(l).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => Array(0x6E, (disp(1) << 4) | visit(r).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => Array(0x78, disp(1) << 4, 0x6A, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: Pos, r: RegByte) => Array(0x6C, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: AbsAddress, r: RegByte) =>
          val abs = visit(l).asInstanceOf[VisitArray].item
          abs(1) match {
            case 8 => Array(0x20 | visit(r).asInstanceOf[VisitInt].item, abs(0))
            case 16 => Array(0x6A, visit(r).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => Array(0x6A, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          Array(0x79, visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: IndirReg, r: RegWord) => Array(0x69, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Disp, r: RegWord) =>
          val disp = visit(l).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => Array(0x6F, (disp(1) << 4) | visit(r).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => Array(0x78, disp(1) << 4, 0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: Pos, r: RegWord) => Array(0x6D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: AbsAddress, r: RegWord) =>
          val abs = visit(l).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => Array(0x6B, visit(r).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => Array(0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          Array(0x7A, visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: IndirReg, r: RegLong) => Array(0x01, 0x00, 0x69, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Disp, r: RegLong) =>
          val disp = visit(l).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => Array(0x01, 0x00, 0x6F, (disp(1) << 4) | visit(r).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => Array(0x01, 0x00, 0x78, disp(1) << 4, 0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: Pos, r: RegLong) => Array(0x01, 0x00, 0x6D, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: AbsAddress, r: RegLong) =>
          val abs = visit(l).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => Array(0x01, 0x00, 0x6B, visit(r).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => Array(0x01, 0x00, 0x6B, 0x20 | visit(r).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: RegByte, r: IndirReg) => Array(0x68, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: Disp) =>
          val disp = visit(r).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => Array(0x6E, 0x80 | (disp(1) << 4) | visit(l).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => Array(0x78, disp(1) << 4, 0x6A, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: RegByte, r: Pre) => Array(0x6C, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: AbsAddress) =>
          val abs = visit(r).asInstanceOf[VisitArray].item
          abs(1) match {
            case 8 => Array(0x30 | visit(l).asInstanceOf[VisitInt].item, abs(0))
            case 16 => Array(0x6A, 0x80 | visit(l).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => Array(0x6A, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: RegWord, r: IndirReg) => Array(0x69, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: Disp) =>
          val disp = visit(r).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => Array(0x6F, 0x80 | (disp(1) << 4) | visit(l).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => Array(0x78, disp(1) << 4, 0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: RegWord, r: Pre) => Array(0x6D, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: AbsAddress) =>
          val abs = visit(r).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => Array(0x6B, 0x80 | visit(l).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => Array(0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
        case (l: RegLong, r: IndirReg) => Array(0x01, 0x00, 0x69, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegLong, r: Disp) =>
          val disp = visit(r).asInstanceOf[VisitArray].item
          disp(2) match {
            case 16 => Array(0x01, 0x00, 0x6F, 0x80 | (disp(1) << 4) | visit(l).asInstanceOf[VisitInt].item, disp(0) >> 8, disp(0))
            case 24 => Array(0x01, 0x00, 0x78, 0x80 | (disp(1) << 4), 0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, disp(0) >> 16, disp(0) >> 8, disp(0))
          }
        case (l: RegLong, r: Pre) => Array(0x01, 0x00, 0x6D, 0x80 | (visit(r).asInstanceOf[VisitInt].item << 4) | visit(l).asInstanceOf[VisitInt].item)
        case (l: RegLong, r: AbsAddress) =>
          val abs = visit(r).asInstanceOf[VisitArray].item
          abs(1) match {
            case 16 => Array(0x01, 0x00, 0x6B, 0x80 | visit(l).asInstanceOf[VisitInt].item, abs(0) >> 8, abs(0))
            case 24 => Array(0x01, 0x00, 0x6B, 0xA0 | visit(l).asInstanceOf[VisitInt].item, 0x00, abs(0) >> 16, abs(0) >> 8, abs(0))
          }
      }

      case Movfpe(left, right) => (left, right) match {
        case (l: AbsAddress, r: RegWord) =>
          val abs = visit(l).asInstanceOf[VisitArray].item(0)
          Array(0x6A, 0x40 | visit(r).asInstanceOf[VisitInt].item, abs << 8, abs)
      }

      case Movtpe(left, right) => (left, right) match {
        case (l: RegWord, r: AbsAddress) =>
          val abs = visit(r).asInstanceOf[VisitArray].item(0)
          Array(0x6A, 0xC0 | visit(l).asInstanceOf[VisitInt].item, abs << 8, abs)
      }

      case Mulxs(left, right) => (left, right) match {
        case (l: RegByte, r: RegWord) => Array(0x01, 0xC0, 0x50, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegLong) => Array(0x01, 0xC0, 0x52, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Mulxu(left, right) => (left, right) match {
        case (l: RegByte, r: RegWord) => Array(0x50, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: RegWord, r: RegLong) => Array(0x52, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Neg(reg) => reg match {
        case r: RegByte => Array(0x17, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x17, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x17, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Nop() => Array(0x00, 0x00)

      case Not(reg) => reg match {
        case r: RegByte => Array(0x17, visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x17, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x17, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Or(left, right) => (left, right) match {
        case (l: Imm, r: RegByte) => Array(0xC0 | visit(r).asInstanceOf[VisitInt].item, visit(l).asInstanceOf[VisitInt].item)
        case (l: RegByte, r: RegByte) => Array(0x14, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegWord) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          Array(0x79, 0x40 | visit(r).asInstanceOf[VisitInt].item, imm >> 8, imm)
        case (l: RegWord, r: RegWord) => Array(0x64, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
        case (l: Imm, r: RegLong) =>
          val imm = visit(l).asInstanceOf[VisitInt].item
          Array(0x7A, 0x40 | visit(r).asInstanceOf[VisitInt].item, imm >> 24, imm >> 16, imm >> 8, imm)
        case (l: RegLong, r: RegLong) => Array(0x01, 0xF0, 0x64, (visit(l).asInstanceOf[VisitInt].item << 4) | visit(r).asInstanceOf[VisitInt].item)
      }

      case Orc(imm) => imm match {
        case i: Imm => Array(0x04, visit(imm).asInstanceOf[VisitInt].item)
      }

      case Pop(reg) => reg match {
        case (r: RegWord) => Array(0x6D, 0x70 | visit(r).asInstanceOf[VisitInt].item)
        case (r: RegLong) => Array(0x01, 0x00, 0x6D, 0x70 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Push(reg) => reg match {
        case (r: RegWord) => Array(0x6D, 0xF0 | visit(r).asInstanceOf[VisitInt].item)
        case (r: RegLong) => Array(0x01, 0x00, 0x6D, 0xF0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rotl(reg) => reg match {
        case r: RegByte => Array(0x12, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x12, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x12, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rotr(reg) => reg match {
        case r: RegByte => Array(0x13, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x13, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x13, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rotxl(reg) => reg match {
        case r: RegByte => Array(0x12, 0x00 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x12, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x12, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rotxr(reg) => reg match {
        case r: RegByte => Array(0x13, 0x00 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x13, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x13, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Rte() => Array(0x56, 0x70)

      case Rts() => Array(0x54, 0x70)

      case Shal(reg) => reg match {
        case r: RegByte => Array(0x10, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x10, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x10, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Shar(reg) => reg match {
        case r: RegByte => Array(0x11, 0x80 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x11, 0x90 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x11, 0xB0 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Shll(reg) => reg match {
        case r: RegByte => Array(0x10, 0x00 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x10, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x10, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Shlr(reg) => reg match {
        case r: RegByte => Array(0x11, 0x00 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegWord => Array(0x11, 0x10 | visit(r).asInstanceOf[VisitInt].item)
        case r: RegLong => Array(0x11, 0x30 | visit(r).asInstanceOf[VisitInt].item)
      }

      case Sleep() => Some(Array(0x01, 0x80))

      case Stc(item) => Some(item match {
        case r: RegByte => visit(r) match {
          case (n: Int) => Array(0x02, r)
        }
        case r: IndirReg => visit(r) match {
          case Some(n: Int) => Array(0x01, 0x40, 0x69, 0x80 | (n << 4))
        }
        case d: Disp => visit(d) match {
          case Some(d: Array[Int]) => d(2) match {
            case 16 => Array(0x01, 0x40, 0x6F, 0x80 | (d(1) << 4), d(0) << 8, d(0))
            case 32 => Array(0x01, 0x40, 0x78, d(1) << 4, 0x6B, 0xA0, 0x00, d(0) << 16, d(0) << 8, d(0))
          }
        }
        case p: Pre => visit(p) match {
          case Some(r: Int) => Array(0x01, 0x40, 0x6D, 0x80 | (r << 4))
        }
        case a: AbsAddress => visit(a) match {
          case Some(abs: Int, s: Int) => s match {
            case 16 => Array(0x01, 0x40, 0x6B, 0x80, abs << 8, abs)
            case 32 => Array(0x01, 0x40, 0x6B, 0xA0, 0x00, abs << 16, abs << 8, abs)
          }
        }
      })

      case Sub(left, right) => Some((left, right) match {
        case (lr: RegByte, rr: RegByte) => (visit(lr), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x18, (l << 4) | r)
        }
        case (i: Imm, rr: RegWord) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, 0x30 | r, l >> 8, l)
        }
        case (lr: RegWord, rr: RegWord) => (visit(lr), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x19, (l << 4) | r)
        }
        case (i: Imm, rr: RegLong) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, 0x30 | r, l >> 24, l >> 16, l >> 8, l)
        }
        case (lr: RegLong, rr: RegLong) => (visit(lr), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1A, 0x80 | (l << 4) | r)
        }
      })

      case Subs(left, right) => Some((left, right) match {
        case (i: Imm, rr: RegLong) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1B, r | (l match {
            case 1 => 0x00
            case 2 => 0x80
            case 4 => 0x90
          }))
        }
      })

      case Subx(left, right) => Some((left, right) match {
        case (i: Imm, rr: RegByte) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0xB0 | r, l)
        }
        case (lr: RegByte, rr: RegByte) => (visit(lr), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x1E, (l << 4) | r)
        }
      })

      case Trapa(imm) => Some(imm match {
        case i: Imm => visit(i) match {
          case Some(n: Int) => Array(0x57, n << 4)
        }
      })

      case Xor(left, right) => Some((left, right) match {
        case (i: Imm, rr: RegByte) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0xD0 | r, l)
        }
        case (lr: RegByte, rr: RegByte) => (visit(lr), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x15, (l << 4) | r)
        }
        case (i: Imm, rr: RegWord) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x79, 0x50 | r, l >> 8, l)
        }
        case (lr: RegWord, rr: RegWord) => (visit(lr), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x65, (l << 4) | r)
        }
        case (i: Imm, rr: RegLong) => (visit(i), visit(rr)) match {
          case (Some(l: Int), Some(r: Int)) => Array(0x7A, 0x50 | r, l >> 24, l >> 16, l >> 8, l)
        }
        case (lr: RegLong, rr: RegLong) => (visit(lr), visit(rr)) match {
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
        case (n: Int) => n
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
        case (Some(d: Int), Some(r: Int)) => Array(disp, reg, size)
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