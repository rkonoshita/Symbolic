package main

import data.DataSet

import scala.collection.mutable.ArrayBuffer

/**
 * Created by rkonoshita on 14/12/11.
 */
class Interrupt {

  def interrupt(d: DataSet): ArrayBuffer[DataSet] = {
    timerB(d.clone())
  }

  def timerB(d: DataSet): ArrayBuffer[DataSet] = {

  }
}
