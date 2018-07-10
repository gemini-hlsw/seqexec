// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.effect.IO
import cats.implicits._
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqexecFailure
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
final case class GDSClient(client: Client[IO], gdsUri: Uri) extends KeywordsClient[IO] with Http4sClientDsl[IO] {
  /**
    * Set the keywords for an image
    */
  override def setKeywords(id: ImageFileId, keywords: KeywordBag, finalFlag: Boolean): SeqActionF[IO, Unit] = {
    val xml = <xml></xml>
    val postRequest = POST(
      gdsUri, xml)
    //   xml,
    //   // <xml></xml>
    //   "String"
    // )
    client.expect[Unit](postRequest).attemptT.leftMap {
      case e: Throwable => SeqexecFailure.GDSException(e, gdsUri)
    }
  }
}
