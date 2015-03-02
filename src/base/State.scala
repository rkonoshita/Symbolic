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
  val condition = data.conset
  //プログラムの終端に達した:true 違う:false
  var error: (Option[Z3Model], Option[String]) = (None, None)

  //スタックエラーを検出
  def stackError(): Boolean = {
    val sp = reg.getLong(7)
    val tsp = sp > 0xFFFFFF80
    val bsp = sp < 0xFFFFFB80
    val bool = tsp || bsp
    val ans = Symbolic.solverCheck(bool.symbol)
    error =
      if (ans) (Some(getModel()), Some("stack error"))
      else (None, None)
    ans
  }

  def divError(): Boolean = {
    if (condition(1)) {
      val z = ccr.getCcr.extract(2, 2).equal(1)
      val ans = Symbolic.solverCheck(z.symbol)
      error =
        if (ans) (Some(getModel()), Some("div0 error"))
        else (None, None)
      ans
    } else false
  }

  //テストケース出力
  def getModel(): Z3Model = {
    Symbolic.sol.push()
    Symbolic.sol.assertCnstr(path.path)
    Symbolic.sol.check()
    val model = Symbolic.sol.getModel()
    Symbolic.sol.pop(1)
    model
  }

  //    override def toString(): String =
  //      if (path.path == null) number + ", " + true
  //      else number + ", " + path.path.toString()

  override def toString(): String = number.toString
}
