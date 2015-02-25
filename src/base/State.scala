package base

import data.DataSet
import z3.scala.Z3Model

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/12/01.
 */
class State(num: Int, data: DataSet, pr: State) {

  val number = num
  val pre = ArrayBuffer[State](pr)
  val next = new ArrayBuffer[State]
  val reg = data.reg
  val mem = data.mem
  val pc = data.pc
  val ccr = data.ccr
  val path = data.path
  val con = data.conset
  private val inNum = mem.in
  private val inBool = mem.ib
  val stop = con(0)
  private val divop = con(1)
  //プログラムの終端に達した:true 違う:false
  var error: (Option[Z3Model], Option[String]) = (None, None)

  //スタックエラーを検出
  def stackError(): Boolean = {
    val sp = reg.getLong(7)
    val tsp = sp > 0xFFFFFF80
    val bsp = sp < 0xFFFFFB80
    Symbolic.sol.assertCnstr(Symbolic.ctx.mkOr(tsp.symbol, bsp.symbol))
    val ans = Symbolic.sol.check.get
    Symbolic.sol.reset
    error =
      if (ans) (Some(getModel), Some("stack error"))
      else (None, None)
    ans
  }

  def divError(): Boolean = {
    if (divop) {
      val z = ccr.getCcr.extract(2, 2).equal(1).simpleify()
      Symbolic.sol.assertCnstr(z.symbol)
      val ans = Symbolic.sol.check.get
      Symbolic.sol.reset
      error =
        if (ans) (Some(getModel), Some("div0 error"))
        else (None, None)
      ans
    } else false
  }

  //テストケース出力
  def getModel(): Z3Model = {
    Symbolic.sol.assertCnstr(path.path)
    Symbolic.sol.check()
    val model = Symbolic.sol.getModel()
    Symbolic.sol.reset()
    model
  }

  //    override def toString(): String =
  //      if (path.path == null) number + ", " + true
  //      else number + ", " + path.path.toString()

  override def toString(): String = number.toString
}
