package main

import java.io.File

import convert.ConvertToInputForm
import data.register.Register
import parser.ASTParser
import z3.scala.{Z3AST, Z3Context}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/11/12.
 */
object Main {

  val ctx = new Z3Context
  var symnum = -1

  def main(args: Array[String]): Unit = {
    println(new ASTParser().parse("ADD.B #3,R0L"))
    new ConvertToInputForm(new File("target"), new File("asm")).convert
  }

  def makeSymbol(size: Int): Z3AST = {
    symnum += 1
    ctx.mkConst("s" + symnum, ctx.mkBVSort(size))
  }

}
