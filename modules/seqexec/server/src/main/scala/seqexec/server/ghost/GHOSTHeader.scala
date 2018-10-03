// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.ghost

import gem.Observation
import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.{InstrumentSystem, SeqAction}
import seqexec.server.keywords._
import seqexec.server.tcs.TcsKeywordsReader

object GHOSTHeader {

  def header(inst: InstrumentSystem[IO],
             gdsClient: GDSClient,
             tcsKeywordsReader: TcsKeywordsReader,
             obsKeywordsReader: ObsKeywordsReader): Header =
    new Header {
      override def sendBefore(obsId: Observation.Id,
                              id: ImageFileId): SeqAction[Unit] =
        SeqAction.void


      override def sendAfter(id: ImageFileId): SeqAction[Unit] =
        SeqAction.void
    }
}
