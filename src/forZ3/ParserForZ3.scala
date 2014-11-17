package forZ3

import scala.collection.GenSeq
import scala.util.parsing.combinator.RegexParsers

/**
 * Created by rkonoshita on 14/11/13.
 */
trait AST

case class mkLet(l: List[AST]) extends AST

case class andLog(l: AST, r: AST) extends AST

case class orLog(l: AST, r: AST) extends AST

case class notLog(n: AST) extends AST

case class equal(l: AST, r: AST) extends AST

case class bvSge(l: AST, r: AST) extends AST

case class bvSgt(l: AST, r: AST) extends AST

case class bvSle(l: AST, r: AST) extends AST

case class bvSlt(l: AST, r: AST) extends AST

case class bvUge(l: AST, r: AST) extends AST

case class bvUgt(l: AST, r: AST) extends AST

case class bvUle(l: AST, r: AST) extends AST

case class bvUlt(l: AST, r: AST) extends AST

case class bvAdd(l: AST, r: AST) extends AST

case class bvSub(l: AST, r: AST) extends AST

case class bvMul(l: AST, r: AST) extends AST

case class bvSdiv(l: AST, r: AST) extends AST

case class bvUdiv(l: AST, r: AST) extends AST

case class bvAnd(l: AST, r: AST) extends AST

case class bvOr(l: AST, r: AST) extends AST

case class bvXor(l: AST, r: AST) extends AST

case class bvAshr(l: AST, r: AST) extends AST

case class bvLshr(l: AST, r: AST) extends AST

case class bvShl(l: AST, r: AST) extends AST

case class bvNot(n: AST) extends AST

case class bvNeg(n: AST) extends AST

case class mkInt(n: Int) extends AST

case class mkSymbol(n: String) extends AST

case class mkVariable(n: String, ast: AST) extends AST

case class findVariable(n: String) extends AST

class ParserForZ3 extends RegexParsers {

  private def root: Parser[AST] = rep(let | expr) ^^ (mkLet(_))

  private def let: Parser[AST] = "(" ~> "let" ~> root <~ ")"

  private def expr: Parser[AST] = expr1 | expr2 | bool | term | variable | ignore

  private def ignore: Parser[AST] = "(" ~> expr <~ ")"

  private def variable: Parser[AST] = "(" ~> """[a-z]+![0-9]+""".r ~ expr <~ ")" ^^ (l => mkVariable(l._1, l._2))

  private def expr1: Parser[AST] = "(" ~> ("and" | "or") ~ expr ~ rep(expr) <~ ")" ^^ {
    case "add" ~ left ~ rep =>
      var ast = left
      rep.foreach(right => ast = andLog(ast, right))
      ast
    case "or" ~ left ~ rep =>
      var ast = left
      rep.foreach(right => ast = orLog(ast, right))
      ast
  }

  private def expr2: Parser[AST] = "(" ~> "not" ~> expr <~ ")" ^^ (notLog(_))

  private def bool: Parser[AST] = "(" ~> ("bvsge" | "bvsgt" | "bvsle" | "bvslt" | "bvuge" | "bvugt" | "bvule" | "bvult" | "=") ~ term ~ term <~ ")" ^^ {
    case "bvsge" ~ left ~ right => bvSge(left, right)
    case "bvsgt" ~ left ~ right => bvSgt(left, right)
    case "bvsle" ~ left ~ right => bvSle(left, right)
    case "bvslt" ~ left ~ right => bvSlt(left, right)
    case "bvuge" ~ left ~ right => bvUge(left, right)
    case "bvugt" ~ left ~ right => bvUgt(left, right)
    case "bvule" ~ left ~ right => bvUle(left, right)
    case "bvult" ~ left ~ right => bvUlt(left, right)
    case "=" ~ left ~ right => equal(left, right)
  }

  private def term: Parser[AST] = term1 | term2 | term3 | elem

  private def term1: Parser[AST] = "(" ~> ("bvadd" | "bvmul" | "bvand" | "bvor" | "bvxor") ~ term ~ rep(term) <~ ")" ^^ {
    case "bvadd" ~ left ~ rep =>
      var ast = left
      rep.foreach(right => ast = bvAdd(ast, right))
      ast
    case "bvmul" ~ left ~ rep =>
      var ast = left
      rep.foreach(right => ast = bvMul(ast, right))
      ast
    case "bvand" ~ left ~ rep =>
      var ast = left
      rep.foreach(right => ast = bvAnd(ast, right))
      ast
    case "bvor" ~ left ~ rep =>
      var ast = left
      rep.foreach(right => ast = bvOr(ast, right))
      ast
    case "bvxor" ~ left ~ rep =>
      var ast = left
      rep.foreach(right => ast = bvXor(ast, right))
      ast
  }

  private def term2: Parser[AST] = "(" ~> ("bvsub" | "bvsdiv" | "bvudiv" | "bvashr" | "bvlshr" | "bvshl") ~ term ~ term <~ ")" ^^ {
    case "bvsub" ~ left ~ right => bvSub(left, right)
    case "bvsdiv" ~ left ~ right => bvSdiv(left, right)
    case "bvudiv" ~ left ~ right => bvUdiv(left, right)
    case "bvashr" ~ left ~ right => bvAshr(left, right)
    case "bvlshr" ~ left ~ right => bvLshr(left, right)
    case "bvshl" ~ left ~ right => bvShl(left, right)
  }

  private def term3: Parser[AST] = "(" ~> ("bvnot" | "bvneg") ~ term <~ ")" ^^ {
    case "bvnot" ~ item => bvNot(item)
    case "bvneg" ~ item => bvNeg(item)
  }

  private def elem: Parser[AST] = num | symbol | hex | alp

  private def num: Parser[AST] = "[0-9]+".r ^^ (n => mkInt(n.toInt))

  private def symbol: Parser[AST] = """s[0-9]+""".r ^^ (mkSymbol(_))

  private def alp: Parser[AST] = """[a-z]+![0-9]+""".r ^^ (findVariable(_))

  private def hex: Parser[AST] = "#x" ~> "[0-9a-f]+".r ^^ (n => mkInt(hexToInt(n)))

  private def skip = ".".r

  private def hexToInt(hex: String): Int = {
    {
      if (hex.length < 8 && !hex.charAt(0).toString.matches("[0-7]")) java.lang.Long.parseLong("f" * (8 - hex.length) + hex, 16)
      else java.lang.Long.parseLong(hex, 16)
    }.toInt
  }

  def parse(input: String) = parseAll(root, input).get
}
