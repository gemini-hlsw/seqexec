// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import seqexec.model.dhs.ImageFileId
import seqexec.server.tcs.TcsKeywordsReader
import seqexec.server.{Header, SeqAction}

final case class GPIHeader(tcsReader: TcsKeywordsReader) extends Header {
  override def sendBefore(id: ImageFileId, inst: String): SeqAction[Unit] =
    SeqAction.void

  override def sendAfter(id: ImageFileId, inst: String): SeqAction[Unit] =
    SeqAction.void
}
