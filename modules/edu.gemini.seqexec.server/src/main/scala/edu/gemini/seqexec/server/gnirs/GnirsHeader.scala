// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.server.gnirs

import edu.gemini.seqexec.model.dhs.ImageFileId
import edu.gemini.seqexec.server.{Header, SeqAction}

class GnirsHeader extends Header {
  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] = SeqAction.void

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] = SeqAction.void
}

object GnirsHeader {
  def apply: GnirsHeader = new GnirsHeader
}
