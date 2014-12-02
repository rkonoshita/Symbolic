package main

import java.io.{File, PrintWriter}

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.io.Source

/**
 * Created by rkonoshita on 14/11/10.
 */

//変換用！
class ConvertToInputForm(t: File, a: File) {

  //ターゲットのプログラムが無いならばエラー
  if (!t.exists) t.mkdir
  val tar = t.listFiles

  //一旦asmフォルダを削除し、作りなおす
  val asm = a
  delete(asm)
  asm.mkdir

  //出現済みローカルラベルを全て保持する
  //現在捜索中のファイルで出現している場合はtrue
  //そうでなければfalse
  val local = new mutable.HashMap[String, Boolean]
  //出現済みラベルを新しいラベルへと写像する
  val trans = new mutable.HashMap[String, String]

  //変換
  def convert() = {
    tar.foreach { file =>
      val source = Source.fromFile(file)
      val writter = new PrintWriter(asm.getAbsolutePath + setFileNameForOS + file.getName.replace(".src", ".asm"))
      source.getLines.foreach { line =>
        //コメント文排除
        val split = line.split(";")
        val t =
          if (!split.isEmpty) split(0).trim
          else ""

        //書き出し
        if (!removeCheck(t)) {

          //被りローカルラベルの捜索
          var str = t
          if (str.matches(".*L[0-9]+.*")) //ラベルの存在判定
            divide(str, "\\s").foreach { l =>
              if (check(l) && !local(l)) //ラベルが登場済み だが 現在捜索中のファイルではない
                str = str.replaceAll(l, trans(l)) //新ラベルへ変換
            }

          //ラベル宣言の直後に文が来ないようにする
          val trim =
            if (str.startsWith("_")) {
              val la = str.split("[ \t]")
              if (la.isEmpty | la.length == 1) str
              else {
                writter.println(la(0))
                var newstr = ""
                for (i <- 1 until la.length) newstr += la(i)
                newstr.trim
              }
            } else str

          //".DATA.(size)は分解
          if (trim.startsWith(".DATA.")) {
            val data = trim.substring(0, 7)
            val dataList = trim.replace(data, "").replaceAll("\\s", "").split(",")
            combine(dataList, data).foreach(writter.println(_))
          } else writter.println(trim)

        }
      }
      //ラベルを未使用状態にする
      local.foreach(l => local(l._1) = false)
      writter.close
      source.close
    }
  }

  //OSの差をちょっと意識してる?
  private def setFileNameForOS: String =
    if (System.getProperty("os.name").startsWith("Windows")) "\\"
    else "/"

  //要らない文をここに定義
  private def removeCheck(str: String): Boolean = {
    str.startsWith(".CPU") |
      str.startsWith(".LINE") |
      str.startsWith(".EXPORT") |
      str.startsWith(".IMPORT") |
      str.startsWith(".END") |
      str.startsWith(".STACK") |
      str.isEmpty
  }

  //フォルダの中身を再帰的に削除
  private def delete(f: File) {
    if (!f.exists()) return
    else if (f.isFile) f.delete
    else f.listFiles.foreach(delete(_))
    f.delete
  }

  //.DATA.(size)以下のデータをバラバラに宣言し直す
  private def combine(list: Array[String], size: String): ListBuffer[String] = {
    val array = new ListBuffer[String]
    list.foreach(l => array += size + " " + l)
    array
  }

  //文をバラバラに分解し、ローカルラベルを全て抽出する
  private def divide(str: String, div: String): mutable.HashSet[String] = {
    val label = new mutable.HashSet[String]
    str.split(div).foreach { s =>
      //ラベルならばlabelに追加
      if (s.matches("L[0-9]+")) label += s
      //ゴミがついている場合は、ゴミでさらに分割
      else if (s.matches("L[0-9]+:.*")) label ++= divide(s, ":")
      else if (s.matches("L[0-9]+\\+.*")) label ++= divide(s, "+")
      else if (s.matches("L[0-9]+-.*")) label ++= divide(s, "-")
      else if (s.matches("L[0-9]+,.*")) label ++= divide(s, ",")
    }
    label
  }

  //すでに存在するラベルならtrue
  //まだなければfalseを返す
  private def check(label: String): Boolean = {
    if (local.contains(label)) {
      //対象ローカルラベル(label)に対して
      //過去のファイルで出現済みだが、変換テーブルには存在しない
      //過去のファイルで出現済みで、変換テーブルにも存在するが、変換先のラベルは現在のファイルではまだ使用されていない
      if (!local(label) && (!trans.contains(label) || !local(trans(label)))) makeLocalLabel(label)
      true
    } else {
      local += label -> true
      false
    }
  }

  //新しいローカルラベルを作っちゃう！
  private def makeLocalLabel(key: String) {
    for (i <- 0 to Byte.MaxValue) {
      val label = "L" + i
      //この番号のローカルラベルがなければ作る
      if (!local.contains(label)) {
        local += label -> true
        trans += key -> label
        return
      }
    }
  }

}
