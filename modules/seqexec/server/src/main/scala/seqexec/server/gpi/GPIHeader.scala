// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gpi

import seqexec.model.dhs.ImageFileId
import seqexec.server.tcs.TcsKeywordsReader
import seqexec.server.{Header, SeqAction}
import seqexec.server.keywords.GDSClient

object GPIHeader {
  def header(client: GDSClient, tcsReader: TcsKeywordsReader): Header = new Header {
    println("GPIHED")
    println(client)
    println(tcsReader)
    override def sendBefore(id: ImageFileId): SeqAction[Unit] = {
      println("Send before")
      SeqAction.void
    }

    override def sendAfter(id: ImageFileId): SeqAction[Unit] =
      SeqAction.void
  }
}
