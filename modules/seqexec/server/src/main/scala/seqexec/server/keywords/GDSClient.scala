// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.effect.Sync
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqActionF
import org.http4s.client.Client
import org.http4s._
import org.http4s.EntityDecoder._
import org.http4s.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.scalaxml._

/**
  * Gemini Data service client
  */
final case class GDSClient[F[_]: Sync](client: Client[F], gdsUri: Uri) extends KeywordsClient[F] with Http4sClientDsl[F] {

  /**
    * Set the keywords for an image
    */
  def setKeywords(id: ImageFileId, keywords: KeywordBag, finalFlag: Boolean): SeqActionF[F, Unit] = {
    val xml = <xml></xml>
    val postRequest = POST(
      gdsUri, xml)
    //   xml,
    //   // <xml></xml>
    //   "String"
    // )
    println(id)
    println(keywords)
    SeqActionF.liftF(client.expect[Unit](postRequest))
  }
}
