package base

import java.io.{File, PrintWriter}

/**
 * Created by ryosuke on 14/12/03.
 */
class ResultWritter {

  //結果のファイル出力
  def write(result: File, time: Long): Unit = {
    val writer = new PrintWriter(result)
    Symbolic.state.foreach { s =>
      //ここで各状態に対して出力する
      writer.println(s.number + ", pc:" + s.pc + "," + s.pre + "," + s.next + ",end:" + s.stop + ",error:" + s.error)
//      writer.println("path:" + s.path)
//      writer.println("ccr:" + s.ccr)
    }
    writer.println(time + "ms")
    writer.close()
  }

}
