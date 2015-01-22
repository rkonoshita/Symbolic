package parser

import java.io.InputStreamReader

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by ryosuke on 14/11/18.
 */

//不完全なパーサ
//必要に応じて規則を追加してください

//ADD.W REGB,REGBが解釈できたりと、割とゆるゆるです
//気になるなら改造してください
//手で*.srcを書かなければ気にしなくていいです
class ASTParser extends RegexParsers {

  def root: Parser[AST] = op | section | mlabel

  def op: Parser[AST] = (add | adds | addx | and | andc | band | bcc | bclr | biand | bild | bior |
    bist | bixor | bld | bnot | bor | bset | bsr | bst | btst | bxor | cmp | daa | das | data | dataBlock | dec |
    divxs | divxu | eepmov | exts | extu | inc | jmp | jsr | ldc | mov | movfpe | movtpe | mulxs | mulxu |
    neg | nop | not | or | orc | pop | push | rotl | rotr | rotxl | rotxr | rte | rts | shal | shar | shll | shlr |
    sleep | stc | sub | subs | subx | trapa | xor | xorc)

  //ADD
  def add: Parser[AST] = "ADD" ~> opsize ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => Add(left, right)
  }

  //ADDS
  def adds: Parser[AST] = "ADDS.L" ~> imm ~ "," ~ reg ^^ {
    case left ~ c ~ right => AddSign(left, right)
  }

  //ADDX
  def addx: Parser[AST] = "ADDX.B" ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => AddExtends(left, right)
  }

  //AND
  def and: Parser[AST] = "AND" ~> opsize ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => And(left, right)
  }

  //ANDC
  def andc: Parser[AST] = "ANDC.B" ~> imm <~ "," <~ "CCR" ^^ (Andc(_))

  //BAND
  def band: Parser[AST] = "BAND.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Band(left, right)
  }

  //条件付き分岐
  def bcc: Parser[AST] = "(BRA|BT|BRN|BF|BHI|BLS|BCC|BHS|BCS|BLO|BNE|BEQ|BVC|BVS|BPL|BMI|BGE|BLT|BGT|BLE)".r ~ label ~ ":" ~ ("8" | "16") ^^ {
    case ("BRA" | "BT") ~ num ~ c ~ size => Bra(num, size.toInt)
    case ("BRN" | "BF") ~ num ~ c ~ size => Brn(num, size.toInt)
    case "BHI" ~ num ~ c ~ size => Bhi(num, size.toInt)
    case "BLS" ~ num ~ c ~ size => Bls(num, size.toInt)
    case ("BCC" | "BHS") ~ num ~ c ~ size => Bcc(num, size.toInt)
    case ("BCS" | "BLO") ~ num ~ c ~ size => Bcs(num, size.toInt)
    case "BNE" ~ num ~ c ~ size => Bne(num, size.toInt)
    case "BEQ" ~ num ~ c ~ size => Beq(num, size.toInt)
    case "BVC" ~ num ~ c ~ size => Bvc(num, size.toInt)
    case "BVS" ~ num ~ c ~ size => Bvs(num, size.toInt)
    case "BPL" ~ num ~ c ~ size => Bpl(num, size.toInt)
    case "BMI" ~ num ~ c ~ size => Bmi(num, size.toInt)
    case "BGE" ~ num ~ c ~ size => Bge(num, size.toInt)
    case "BLT" ~ num ~ c ~ size => Blt(num, size.toInt)
    case "BGT" ~ num ~ c ~ size => Bgt(num, size.toInt)
    case "BLE" ~ num ~ c ~ size => Ble(num, size.toInt)
  }

  //BCLR
  def bclr: Parser[AST] = "BCLR.B" ~> (imm | reg) ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bset(left, right)
  }

  //BIAND
  def biand: Parser[AST] = "BIAND.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Biand(left, right)
  }

  //BILD
  def bild: Parser[AST] = "BILD.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bild(left, right)
  }

  //BIOR
  def bior: Parser[AST] = "BIOR.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bior(left, right)
  }

  //BIST
  def bist: Parser[AST] = "BIST.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bist(left, right)
  }

  //BIXOR
  def bixor: Parser[AST] = "BIXOR.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bixor(left, right)
  }

  //BLD
  def bld: Parser[AST] = "BLD.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bld(left, right)
  }

  //BNOT
  def bnot: Parser[AST] = "BNOT.B" ~> (imm | reg) ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bnot(left, right)
  }

  //BOR
  def bor: Parser[AST] = "BOR.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bor(left, right)
  }

  //BSET
  def bset: Parser[AST] = "BSET.B" ~> (imm | reg) ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bset(left, right)
  }

  //BSR
  def bsr: Parser[AST] = "BSR" ~> label ~ ":" ~ ("8" | "16") ^^ {
    case label ~ c ~ size => Bsr(label, size.toInt)
  }

  //BST
  def bst: Parser[AST] = "BST.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bst(left, right)
  }

  //BTST
  def btst: Parser[AST] = "BTST.B" ~> (imm | reg) ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Btst(left, right)
  }

  //BXOR
  def bxor: Parser[AST] = "BXOR.B" ~> imm ~ "," ~ (reg | indirReg | abs) ^^ {
    case left ~ c ~ right => Bxor(left, right)
  }

  //CMP
  def cmp: Parser[AST] = "CMP" ~> opsize ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => Cmp(left, right)
  }

  //DAA
  def daa: Parser[AST] = "DAA.B" ~> reg ^^ (Daa(_))

  //DAS
  def das: Parser[AST] = "DAS.B" ~> reg ^^ (Das(_))

  //DEC
  def dec: Parser[AST] = "DEC" ~> opsize ~> (imm | reg) ~ ",".? ~ reg.? ^^ {
    case left ~ c ~ right =>
      right match {
        case Some(s: AST) => Dec(left, s)
        case None => Dec(left, Empty())
      }
  }

  //DIVXS
  def divxs: Parser[AST] = "DIVXS" ~> opsize ~> reg ~ "," ~ reg ^^ {
    case left ~ c ~ right => Divxs(left, right)
  }

  //DIVXU
  def divxu: Parser[AST] = "DIVXU" ~> opsize ~> reg ~ "," ~ reg ^^ {
    case left ~ c ~ right => Divxu(left, right)
  }

  //EEPMOV
  def eepmov: Parser[AST] = "EEPMOV" ~> opsize ^^ {
    case "B" => Eepmov(8)
    case "W" => Eepmov(16)
  }

  //EXTS
  def exts: Parser[AST] = "EXTS" ~> opsize ~> reg ^^ (Exts(_))

  //EXTU
  def extu: Parser[AST] = "EXTU" ~> opsize ~> reg ^^ (Extu(_))

  //INC
  def inc: Parser[AST] = "INC" ~> opsize ~> (imm | reg) ~ ",".? ~ reg.? ^^ {
    case left ~ c ~ right =>
      right match {
        case Some(s: AST) => Inc(left, s)
        case None => Inc(left, Empty())
      }
  }

  //JMP
  def jmp: Parser[AST] = "JMP" ~> (indirReg | abs | indirMem) ^^ (Jmp(_))

  //JSR
  def jsr: Parser[AST] = "JSR" ~> (abs | indirReg | indirMem) ^^ (Jsr(_))

  //LDC
  def ldc: Parser[AST] = "LDC" ~> opsize ~> (imm | reg | indirReg | disp | pos | abs) <~ "," <~ "CCR" ^^ (Ldc(_))

  //MOV
  def mov: Parser[AST] = "MOV" ~> opsize ~> (reg | imm | indirReg | disp | pos | abs) ~ "," ~ (reg | indirReg | disp | pos | abs) ^^ {
    case left ~ c ~ right => Mov(left, right)
  }

  //MOVFPE
  def movfpe: Parser[AST] = "MOVFPE.B" ~> abs ~ "," ~ reg ^^ {
    case left ~ c ~ right => Movfpe(left, right)
  }

  //MOVTPE
  def movtpe: Parser[AST] = "MOVTPE.B" ~> reg ~ "," ~ abs ^^ {
    case left ~ c ~ right => Movfpe(left, right)
  }

  //MULXS
  def mulxs: Parser[AST] = "MULXS" ~> opsize ~> reg ~ "," ~ reg ^^ {
    case left ~ c ~ right => Mulxs(left, right)
  }

  //MULXU
  def mulxu: Parser[AST] = "MULXU" ~> opsize ~> reg ~ "," ~ reg ^^ {
    case left ~ c ~ right => Mulxu(left, right)
  }

  //NEG
  def neg: Parser[AST] = "NEG" ~> opsize ~> reg ^^ (Neg(_))

  //NOP
  def nop: Parser[AST] = "NOP" ^^ {
    case _ => Nop()
  }

  //NOT
  def not: Parser[AST] = "NOT" ~> opsize ~> reg ^^ (Not(_))

  //OR
  def or: Parser[AST] = "OR" ~> opsize ~> reg ~ "," ~ reg ^^ {
    case left ~ c ~ right => Or(left, right)
  }

  //ORC
  def orc: Parser[AST] = "ORC.B" ~> imm <~ "," <~ "CCR" ^^ (Orc(_))

  //POP
  def pop: Parser[AST] = "POP" ~> opsize ~> reg ^^ (Pop(_))

  //PUSH
  def push: Parser[AST] = "PUSH" ~> opsize ~> reg ^^ (Push(_))

  //ROTL
  def rotl: Parser[AST] = "ROTL" ~> opsize ~> reg ^^ (Rotl(_))

  //ROTR
  def rotr: Parser[AST] = "ROTR" ~> opsize ~> reg ^^ (Rotr(_))

  //ROTXL
  def rotxl: Parser[AST] = "ROTXL" ~> opsize ~> reg ^^ (Rotxl(_))

  //ROTXR
  def rotxr: Parser[AST] = "ROTXR" ~> opsize ~> reg ^^ (Rotxr(_))

  //RTE
  def rte: Parser[AST] = "RTE" ^^ {
    case _ => Rte()
  }

  //RTS
  def rts: Parser[AST] = "RTS" ^^ {
    case _ => Rts()
  }

  //SHAL
  def shal: Parser[AST] = "SHAL" ~> opsize ~> reg ^^ (Shal(_))

  //SHAR
  def shar: Parser[AST] = "SHAL" ~> opsize ~> reg ^^ (Shar(_))

  //SHLL
  def shll: Parser[AST] = "SHAL" ~> opsize ~> reg ^^ (Shll(_))

  //SHLR
  def shlr: Parser[AST] = "SHAL" ~> opsize ~> reg ^^ (Shlr(_))

  //SLEEP
  def sleep: Parser[AST] = "SLEEP" ^^ {
    case _ => Sleep()
  }

  //STC
  def stc: Parser[AST] = "STC" ~> opsize ~> "CCR" ~> "," ~> (reg | indirReg | disp | pos | abs) ^^ (Stc(_))

  //SUB
  def sub: Parser[AST] = "SUB" ~> opsize ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => Sub(left, right)
  }

  //SUBS
  def subs: Parser[AST] = "SUBS.L" ~> imm ~ "," ~ reg ^^ {
    case left ~ c ~ right => Subs(left, right)
  }

  //SUBX
  def subx: Parser[AST] = "SUBX.B" ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => Subx(left, right)
  }

  //TRAPA
  def trapa: Parser[AST] = "TRAPA" ~> imm ^^ (Trapa(_))

  //XOR
  def xor: Parser[AST] = "XOR" ~> opsize ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => Xor(left, right)
  }

  //XORC
  def xorc: Parser[AST] = "XORC.B" ~> opsize ~> imm <~ "," <~ "CCR" ^^ (Xorc(_))

  //オペレーションのサイズ
  def opsize = "." ~> ("B" | "W" | "L")

  //現在のセクション
  def section: Parser[AST] = ".SECTION" ~> "[VPCDRB]".r <~ "," <~ ("CODE" | "DATA") <~ "," <~ "ALIGN" <~ "=" <~ "[0-9]+".r ^^ (Section(_))

  //レジスタ
  def reg: Parser[AST] = (regByte | regWord | regLong | "SP") ^^ {
    case s: AST => s
    case s: String => RegLong(7)
  }

  def regLong: Parser[AST] = "ER" ~> "[0-7]".r ^^ {
    case num => RegLong(num.toInt)
  }

  def regWord: Parser[AST] = ("E" | "R") ~ "[0-7]".r ^^ {
    case "E" ~ num => RegWord(num.toInt | 0x08)
    case "R" ~ num => RegWord(num.toInt)
  }

  def regByte: Parser[AST] = "R" ~> "[0-7]".r ~ ("H" | "L") ^^ {
    case num ~ "H" => RegByte(num.toInt)
    case num ~ "L" => RegByte(num.toInt | 0x80)
  }

  //スタックポインタ(register7)
  def sp: Parser[AST] = "SP" ^^ {
    case _ => RegLong(7)
  }

  //計算
  def expr: Parser[AST] = expr6

  //計算優先度1（高)
  def expr1: Parser[AST] = ("+" | "-" | "~" | "HIGH" | "LOW" | "HWORD" | "LWORD").? ~ number ^^ {
    case op ~ num =>
      op match {
        case Some(s) =>
          op.get match {
            case "+" => num
            case "-" => Minus(num)
            case "HIGH" => High(num)
            case "LOW" => Low(num)
            case "HWORD" => HWord(num)
            case "LWORD" => LWord(num)
          }
        case None => num
      }
  }

  //計算優先度2
  def expr2: Parser[AST] = {
    chainl1(expr1, expr1, calc("*") | calc("/"))
  }

  //計算優先度3
  def expr3: Parser[AST] = {
    chainl1(expr2, expr2, calc("+") | calc("-"))
  }

  //計算優先度4
  def expr4: Parser[AST] = {
    chainl1(expr3, expr3, calc("<<") | calc(">>"))
  }

  //計算優先度5
  def expr5: Parser[AST] = {
    chainl1(expr4, expr4, calc("&"))
  }

  //計算優先度6（低）
  def expr6: Parser[AST] = {
    chainl1(expr5, expr5, calc("|") | calc("~"))
  }

  //計算
  def calc(op: String) = op ^^ {
    case op => (left: AST, right: AST) => Expr(op, left, right)
  }

  //数値として使えるものを一括
  def number: Parser[AST] = sizeof | startof | num | hex | label | "(" ~> expr <~ ")"

  //10進数
  def num: Parser[AST] = "[0-9]+".r ^^ (num => Number(num.toInt))

  //16進数
  def hex: Parser[AST] = "H'" ~> "[0-9A-F]+".r ^^ (num => Number(hexToInt(num)))

  //2進数,8進数は未実装

  //ラベル
  def label: Parser[AST] = ("_[a-zA-Z0-9_$]+".r | "L[0-9]+".r) ^^ (LabelName(_))

  //位置として出現するラベル
  def mlabel: Parser[AST] = label <~ ":".? ^^ (MakeLabel(_))

  //セクションサイズ
  def sizeof: Parser[AST] = "SIZEOF" ~> "[VPCDBR]".r ^^ (SizeOf(_))

  //セクション開始位置
  def startof: Parser[AST] = "STARTOF" ~> "[VPCDBR]".r ^^ (StartOf(_))

  //絶対アドレス
  def abs: Parser[AST] = "@" ~> (expr | hex | label) ~ ":" ~ ("8" | "16" | "24").? ^^ {
    case num ~ c ~ size =>
      size match {
        case Some(s) => AbsAddress(num, size.get.toInt)
        case None => AbsAddress(num, 16)
      }
  }

  //イミディエイト
  def imm: Parser[AST] = "#" ~> expr <~ ":(8|16|32)".? ^^ (Imm(_))

  //レジスタ間接
  def indirReg: Parser[AST] = "@" ~> reg ^^ (IndirReg(_))

  //メモリ間接
  def indirMem: Parser[AST] = "@" ~> abs ^^ (IndirAdd(_))

  //ディスプレースメント付
  def disp: Parser[AST] = "@" ~> "(" ~> (expr | hex) ~ ":" ~ ("16" | "24") ~ "," ~ reg <~ ")" ^^ {
    case num ~ c1 ~ size ~ c2 ~ reg => Disp(num, reg, size.toInt)
  }

  //ポストインクリメント
  //プリディクリメント
  def pos: Parser[AST] = "@" ~> (reg ~ "+" | "-" ~ reg) ^^ {
    case left ~ right =>
      (left, right) match {
        case (l: AST, r: String) => Pos(l)
        case (l: String, r: AST) => Pre(r)
      }
  }

  //16進数(文字列)を10進数（整数）へ変換
  private def hexToInt(hex: String): Int = {
    {
      if (hex.length < 8 && !hex.charAt(0).toString.matches("[0-7]")) java.lang.Long.parseLong("f" * (8 - hex.length) + hex, 16)
      else java.lang.Long.parseLong(hex, 16)
    }.toInt
  }

  def data: Parser[AST] = ".DATA" ~> opsize ~ expr ^^ {
    case size ~ expr =>
      size match {
        case "B" => Data(expr, 8)
        case "W" => Data(expr, 16)
        case "L" => Data(expr, 32)
      }
  }

  def dataBlock: Parser[AST] = ".DATAB" ~> opsize ~ expr ~ "," ~ expr ^^ {
    case size ~ num1 ~ c ~ num2 =>
      size match {
        case "B" => DataBlock(num1, num2, 8)
        case "W" => DataBlock(num1, num2, 16)
        case "L" => DataBlock(num1, num2, 32)
      }
  }

  def parse(in: InputStreamReader) = parseAll(root, in)

  def parse(in: String) = parseAll(root, in)

}
