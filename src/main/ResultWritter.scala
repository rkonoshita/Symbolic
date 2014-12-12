package main

import java.io.{PrintWriter, FileWriter, BufferedWriter, File}

/**
 * Created by ryosuke on 14/12/03.
 */
class ResultWritter {

  def write(result: File): Unit = {
    val writer = new PrintWriter(result)
    Main.state.foreach { s =>
      writer.println(s.number + ", pc:" + s.pc + "," + s.pre + "," + s.next + "," + "check:" + s.pathCheck + ",end:" + s.stop)
    }
    writer.close
  }

}
