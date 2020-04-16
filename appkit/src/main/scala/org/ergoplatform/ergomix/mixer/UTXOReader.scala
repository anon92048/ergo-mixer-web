package org.ergoplatform.ergomix.mixer

import org.ergoplatform.ergomix.mixer.Models.OutBox

trait UTXOReader {
  def getUnspentBoxes(address:String):Seq[OutBox]
}
