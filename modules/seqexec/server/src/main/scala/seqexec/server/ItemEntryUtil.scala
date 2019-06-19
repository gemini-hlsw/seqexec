// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import edu.gemini.spModel.`type`.{DisplayableSpType, LoggableSpType, SequenceableSpType}
import edu.gemini.spModel.config2.ItemEntry

/**
 * Created by jluhrs on 4/27/15.
 */
object ItemEntryUtil {

  def showItems(ks: Array[ItemEntry]): String = {

    def width(ks: Array[ItemEntry]): Int = ks match {
      case Array() => 0
      case _ => ks.maxBy(_.getKey.toString.length).toString.length
    }

    def seqValue(o: Object): String = o match {
      case s: SequenceableSpType => s.sequenceValue()
      case d: DisplayableSpType  => d.displayValue()
      case l: LoggableSpType     => l.logValue()
      case _                     => o.toString
    }

    val pad = width(ks)
    (ks.sortWith((u, v) => u.getKey.compareTo(v.getKey) < 0) map { p =>
      val paddedKey = s"%-${pad}s".format(p.getKey.toString)
      s"$paddedKey -> ${seqValue(p.getItemValue)}"
    }).mkString(s"\n", "\n", "")
  }

}
