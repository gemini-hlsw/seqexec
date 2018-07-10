// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.effect.IO
import cats.data.EitherT
import cats.implicits._
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqexecFailure
import seqexec.server.SeqActionF
import org.http4s.client.Client
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.scalaxml._
import scala.xml.Elem

/**
  * Gemini Data service client
  */
final case class GDSClient(client: Client[IO], gdsUri: Uri) extends KeywordsClient[IO] with Http4sClientDsl[IO] {
  // Build an xml rpc request to store keywords
  private def storeKeywords(id: ImageFileId, ks: KeywordBag): Elem =
    <methodCall>
      <methodName>HeaderReceiver.storeKeywords</methodName>
      <params>
        <param>
          <value>
            <string>{id}</string>
          </value>
        </param>
        <param>
          <value>
            <array>
              <data>
                {
                  for {
                    k <- ks.keywords
                  } yield <value><string>{s"${k.name},${k.keywordType.gdsType},${k.value}"}</string></value>
                }
              </data>
            </array>
          </value>
        </param>
      </params>
    </methodCall>

  /**
    * Set the keywords for an image
    */
  override def setKeywords(id: ImageFileId, ks: KeywordBag, finalFlag: Boolean): SeqActionF[IO, Unit] = {
    // Build the request
    val xmlRpc = storeKeywords(id, ks)
    val postRequest = POST(gdsUri, xmlRpc)

    // Do the request
    client.expect[Elem](postRequest)(scalaxml.xml).attemptT.leftMap {
      case e: Throwable => SeqexecFailure.GDSException(e, gdsUri): SeqexecFailure
    }.flatMap(xml => EitherT.fromEither(GDSClient.checkError(xml, gdsUri)))
  }
}

object GDSClient {
  def checkError(e: Elem, gdsUri: Uri): Either[SeqexecFailure, Unit] = {
    val v = for {
      m <- e \\ "methodResponse" \ "fault" \ "value" \ "struct" \\ "member"
      if ((m \ "name").text === "faultString")
    } yield (m \ "value").text.trim
    v.headOption.fold(().asRight[SeqexecFailure])(SeqexecFailure.GDSXMLError(_, gdsUri).asLeft[Unit])
  }
}
