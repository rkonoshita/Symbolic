package parser

import java.io.{InputStreamReader, BufferedReader}

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

  //ADD
  def add: Parser[AST] = "ADD." ~> opsize ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => Add(left, right)
  }

  //INC
  def inc: Parser[AST] = "INC." ~> opsize ~> (imm | reg) ~ ",".? ~ reg.? ^^ {
    case left ~ c ~ right =>
      right match {
        case Some(s) => Inc(left, s.asInstanceOf[AST])
        case None => Inc(left)
      }
  }

  //CMP
  def cmp: Parser[AST] = "CMP." ~> opsize ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => Cmp(left, right)
  }

  //SUB
  def sub: Parser[AST] = "SUB." ~> opsize ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => Sub(left, right)
  }

  //AND
  def and: Parser[AST] = "AND." ~> opsize ~> (imm | reg) ~ "," ~ reg ^^ {
    case left ~ c ~ right => And(left, right)
  }

  //NOT
  def not: Parser[AST] = "NOT." ~> opsize ~> reg ^^ (Not(_))

  //ANDC
  def andc: Parser[AST] = "ANDC.B" ~> imm <~ "," <~ "CCR" ^^ (Andc(_))

  //ORC
  def orc: Parser[AST] = "ORC.B" ~> imm <~ "," <~ "CCR" ^^ (Orc(_))

  //MOV
  def mov: Parser[AST] = "MOV." ~> opsize ~> (imm | reg | abs | disp) ~ "," ~ reg ^^ {
    case left ~ c ~ right => Mov(left, right)
  }

  //PUSH
  def push: Parser[AST] = "PUSH." ~> opsize ~> reg ^^ (Push(_))

  //POP
  def pop: Parser[AST] = "POP." ~> opsize ~> reg ^^ (Pop(_))

  //EXTU
  def extu: Parser[AST] = "EXTU." ~> opsize ~> reg ^^ (Extu(_))

  //BSET
  def bset: Parser[AST] = "BSET.B" ~> (imm | reg) ~ "," ~ (reg | abs | indirReg) ^^ {
    case left ~ c ~ right => Bset(left, right)
  }

  //BCLR
  def bclr: Parser[AST] = "BCLR.B" ~> (imm | reg) ~ "." ~ (reg | abs | indirReg) ^^ {
    case left ~ c ~ right => Bset(left, right)
  }

  //JSR
  def jumpSub: Parser[AST] = "JSR" ~> (abs | indirReg | indirMem) ^^ (Jsr(_))

  //JMP
  def jump: Parser[AST] = "JMP" ~> (indirReg | abs | indirMem) ^^ (Jmp(_))

  //RTS
  def rts: Parser[AST] = "RTS" ^^ {
    case _ => Rts()
  }

  def rte: Parser[AST] = "RTE" ^^ {
    case _ => Rte()
  }

  //条件付き分岐
  def bcc: Parser[AST] = "(BRA|BLO|BLT|BHI)".r ~ label <~ ":(8|16)".r ^^ {
    case "BRA" ~ num => Bra(num)
    case "BLO" ~ num => Blo(num)
    case "BLT" ~ num => Blt(num)
    case "BHI" ~ num => Bhi(num)
  }

  //オペレーションのサイズ
  def opsize = ("B" | "W" | "L")

  //現在のセクション
  def section: Parser[AST] = ".SECTION" ~> "[VPCDRB]".r <~ "," <~ "CODE" <~ "," <~ "ALIGN" <~ "=" <~ "[0-9]+".r ^^ (Section(_))

  //レジスタ
  def reg: Parser[AST] = ("ER" | "R" | "E") ~ "[0-7]".r ~ ("H" | "L").? ^^ {
    case str1 ~ num ~ str2 =>
      str1 match {
        case "ER" => RegLong(num.toInt)
        case "E" => RegWord(num.toInt | 0x08)
        case "R" =>
          str2 match {
            case Some(s) =>
              s match {
                case "H" => RegByte(num.toInt)
                case "L" => RegByte(num.toInt | 0x08)
              }
            case None => RegWord(num.toInt)
          }
      }
  }

  //スタックポインタ(register7)
  def sp: Parser[AST] = "SP" ^^ {
    case _ => RegLong(7)
  }

  //計算
  def expr: Parser[AST] = expr6

  //計算優先度1（高)
  def expr1: Parser[AST] = ("+" | "-" | "HIGH" | "LOW" | "HWORD" | "LWORD").? ~ number ^^ {
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
  def number: Parser[AST] = num | hex | label | "(" ~> expr <~ ")"

  //10進数
  def num: Parser[AST] = "[0-9]+".r ^^ (num => Number(num.toInt))

  //16進数
  def hex: Parser[AST] = "H'" ~> "[0-9A-F]+".r ^^ (num => Number(hexToInt(num)))

  //2進数,8進数は未実装

  //ラベル
  def label: Parser[AST] = ("_[a-zA-Z0-9_$]+".r | "L[0-9]+".r) ^^ (Label(_))

  //位置として出現するラベル
  def mlabel: Parser[AST] = label <~ ":" ^^ (MakeLabel(_))

  //絶対アドレス
  def abs: Parser[AST] = "@" ~> (expr | hex | label) ~ ":(8|16|24)".? ^^ {
    case num ~ size =>
      size match {
        case Some(s) => AbsAddress(num, size.get.replace(":", "").toInt)
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
  def disp: Parser[AST] = "@" ~> "(" ~> (expr | hex) ~ ":(16|24)".r ~ "," ~ reg <~ ")" ^^ {
    case num ~ size ~ c ~ reg => Disp(num, reg, size.replace(":", "").toInt)
  }

  //16進数(文字列)を10進数（整数）へ変換
  private def hexToInt(hex: String): Int = {
    {
      if (hex.length < 8 && !hex.charAt(0).toString.matches("[0-7]")) java.lang.Long.parseLong("f" * (8 - hex.length) + hex, 16)
      else java.lang.Long.parseLong(hex, 16)
    }.toInt
  }

  def data: Parser[AST] = ".DATA" ~> opsize ~> expr ^^ (Data(_))

  def dataBlock: Parser[AST] = ".DATAB." ~> opsize ~> expr ~ expr ^^ {
    case num1 ~ num2 => DataBlock(num1, num2)
  }

  def parse(in: InputStreamReader) = parseAll(expr, in)

}
