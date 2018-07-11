// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import knobs.{CfgText, CfgValue, Configured}
import gem.enum.Site

trait SeqexecConfiguration {

  implicit val configuredSite: Configured[Site] = new Configured[Site] {

    def apply(a: CfgValue) = a match {
      case CfgText(t) => Site.fromTag(t)
      case _          => None
    }
  }

}
