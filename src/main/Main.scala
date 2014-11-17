package main

import data.register.Register
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable

/**
 * Created by rkonoshita on 14/11/12.
 */
object Main {

  val ctx = new Z3Context
  var symnum = -1

  def main(args: Array[String]): Unit = {


  }

  def makeSymbol(size: Int): Z3AST = {
    symnum += 1
    ctx.mkConst("s" + symnum, ctx.mkBVSort(size))
  }

}
