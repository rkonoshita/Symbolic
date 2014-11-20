package parser

import scala.util.parsing.combinator.RegexParsers

/**
 * Created by ryosuke on 14/11/18.
 */
class ASTParser extends RegexParsers {

  //ADD
  def add: Parser[AST] = "ADD." ~> opsize ~ (imm | reg) ~ "," ~ reg ^^ {
    case size ~ left ~ c ~ right =>
      size match {
        case "B" => AddByte(left, right)
        case "W" => AddWord(left, right)
        case "L" => AddLong(left, right)
      }
  }

//  //INC
//  def inc: Parser[AST] = "INC." ~> opsize ~ (imm | reg) ~ "," ~ reg ^^ {
//    case size ~ left ~ c ~ right =>
//      size match {
//        case INC
//      }
//  }

  //MOV
  def mov: Parser[AST] = "MOV." ~> opsize ~ (imm | reg | abs | disp) ~ "," ~ reg ^^ {
    case size ~ left ~ c ~ right =>
      size match {
        case "B" => MovByte(left, right)
        case "W" => MovWord(left, right)
        case "L" => MovLong(left, right)
      }
  }

  //SUB
  def sub: Parser[AST] = "SUB." ~> opsize ~ (imm | reg) ~ "," ~ reg ^^ {
    case size ~ left ~ c ~ right =>
      size match {
        case "B" => SubByte(left, right)
        case "W" => SubWord(left, right)
        case "L" => SubLong(left, right)
      }
  }

  //JSR
  def jumpSub: Parser[AST] = "JSR" ~> (abs | indirReg | indirMem) ^^ (JumpSub(_))

  //オペレーションのサイズ
  def opsize: Parser[String] = ("B" | "W" | "L") ^^ (str => str)

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

  //条件付き分岐
  def bcc: Parser[AST] = "BRA" ~ ulabel <~ ":(8|16)".r ^^ {
    case "BRA" ~ num => BRA(num)
  }

  //10進数
  def num: Parser[AST] = "[0-9]+".r ^^ (num => Number(num.toInt))

  //16進数
  def hex: Parser[AST] = "H'" ~> "[0-9A-F]+".r ^^ (num => Number(hexToInt(num)))

  //2進数,8進数は未実装

  //位置として出現するラベル
  def mlabel: Parser[AST] = ("_[a-zA-Z0-9_$]+".r | "L[0-9]+".r) <~ ":" ^^ (MakeLabel(_))

  //参照として出現するラベル
  def ulabel: Parser[AST] = ("_[a-zA-Z0-9_$]+".r | "L[0-9]+".r) ^^ (UseLabel(_))

  //絶対アドレス
  def abs: Parser[AST] = "@" ~> (num | hex | ulabel) ~ ":(8|16|24)".? ^^ {
    case num ~ size =>
      size match {
        case Some(s) => AbsAddress(num, size.get.replace(":", "").toInt)
        case None => AbsAddress(num, 16)
      }

  }

  //イミディエイト
  def imm: Parser[AST] = "#" ~> (num | hex) <~ ":(8|16|32)".? ^^ (Imm(_))

  //レジスタ間接
  def indirReg: Parser[AST] = "@" ~> reg ^^ (IndirReg(_))

  //メモリ間接
  def indirMem: Parser[AST] = "@" ~> abs ^^ (IndirAdd(_))

  //ディスプレースメント付
  def disp: Parser[AST] = "@" ~> "(" ~> (num | hex) ~ ":(16|24)".r ~ "," ~ reg <~ ")" ^^ {
    case num ~ size ~ c ~ reg => Disp(num, reg, size.replace(":", "").toInt)
  }

  //16進数(文字列)を10進数（整数）へ変換
  private def hexToInt(hex: String): Int = {
    {
      if (hex.length < 8 && !hex.charAt(0).toString.matches("[0-7]")) java.lang.Long.parseLong("f" * (8 - hex.length) + hex, 16)
      else java.lang.Long.parseLong(hex, 16)
    }.toInt
  }

  def parse(in: String) = parseAll(add, in)

}
