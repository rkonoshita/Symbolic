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
  val reach = //到達可能:true 到達不能:false
    if (path.path == null) true
    else {
      Symbolic.sol.assertCnstr(path.path)
      val ans = Symbolic.sol.check.get
      Symbolic.sol.reset
      ans
    }
  val stop = data.stop //プログラムの終端に達した:true 違う:false
  var error: (Boolean, Option[Z3Model]) = (false, None)

  //スタックエラーを検出
  def stackError(): (Boolean, Option[Z3Model]) = {
    val sp = reg.getLong(7)
    val tsp = sp > 0xFFFFFF80
    val bsp = sp < 0xFFFFFB80
    Symbolic.sol.assertCnstr(Symbolic.ctx.mkOr(tsp.symbol, bsp.symbol))
    val ans = Symbolic.sol.check.get
    Symbolic.sol.reset
    val model =
      if (ans) Some(getModel)
      else None
    error = (ans, model)
    error
  }

  //テストケース出力
  def getModel():Z3Model = {
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
