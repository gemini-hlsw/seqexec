// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import gem.Observation
import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords.Header
import seqexec.server.SeqAction
import seqexec.server.keywords.GDSClient

object GPIHeader {

  def header(gdsClient: GDSClient): Header = new Header {
    override def sendBefore(obsId: Observation.Id, id: ImageFileId): SeqAction[Unit] =
      gdsClient.openObservation(obsId, id)

    override def sendAfter(obsId: Observation.Id, id: ImageFileId): SeqAction[Unit] =
      SeqAction.void
  }
}
