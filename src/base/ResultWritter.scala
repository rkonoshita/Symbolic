package base

import java.io.{File, PrintWriter}

/**
 * Created by ryosuke on 14/12/03.
 */
class ResultWritter {

  def write(result: File, time: Long): Unit = {
    val writer = new PrintWriter(result)
    Symbolic.state.foreach { s =>
      writer.println(s.number + ", pc:" + s.pc + "," + s.pre + "," + s.next + "," + "reach:" + s.reach + ",end:" + s.stop + ",error:" + s.error)
    }
    writer.println(time + "ms")
    writer.close
  }

}
