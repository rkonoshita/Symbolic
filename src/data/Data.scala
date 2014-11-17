package data

import data.register.{Memory, Register}

/**
 * Created by rkonoshita on 14/11/12.
 */
class Data(r: Register, m: Memory) {

  val reg = r
  val mem = m

}
