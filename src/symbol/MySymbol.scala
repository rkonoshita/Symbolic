package symbol

import z3.scala.Z3AST

/**
 * Created by rkonoshita on 14/11/26.
 */
trait MySymbol {
  type T
  val symbol: T
}

class CtxSymbol(ast: Z3AST) {
  type T = Z3AST
  val symbol = ast
}

class IntSymbol(ast: Int) {
  type T = Int
  val symbol = ast
}