// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.effect.IO
import cats.data.EitherT
import cats.implicits._
import gem.Observation
import org.http4s.client.Client
import org.http4s._
import org.http4s.dsl.io._
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.scalaxml._
import scala.xml.Elem
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqexecFailure
import seqexec.server.SeqActionF

/**
  * Gemini Data service client
  */
final case class GDSClient(client: Client[IO], gdsUri: Uri)
    extends Http4sClientDsl[IO] {

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
        {keywordsParam(ks)}
      </params>
    </methodCall>

  private def handleConnectionError(e: Throwable): SeqexecFailure =
    SeqexecFailure.GDSException(e, gdsUri)

  private def handleXmlError(xml: Elem): SeqActionF[IO, Unit] =
    EitherT.fromEither(GDSClient.checkError(xml, gdsUri))

  /**
    * Set the keywords for an image
    */
  def setKeywords(id: ImageFileId, ks: KeywordBag): SeqActionF[IO, Unit] = {
    // Build the request
    val xmlRpc      = storeKeywords(id, ks)
    val postRequest = POST(gdsUri, xmlRpc)

    // Do the request
    client
      .expect[Elem](postRequest)(scalaxml.xml)
      .attemptT
      .leftMap(handleConnectionError)
      .flatMap(handleXmlError)
  }

  // Build an xml rpc request to open an observation
  private def openObservationRPC(obsId: Observation.Id,
                                 id: ImageFileId,
                                 ks: KeywordBag): Elem =
    <methodCall>
      <methodName>HeaderReceiver.openObservation</methodName>
      <params>
        <param>
          <value>
            <string>{obsId.format}</string>
          </value>
        </param>
        <param>
          <value>
            <string>{id}</string>
          </value>
        </param>
        {keywordsParam(ks)}
      </params>
    </methodCall>

  def openObservation(obsId: Observation.Id,
                      id: ImageFileId,
                      ks: KeywordBag): SeqActionF[IO, Unit] = {
    // Build the request
    val xmlRpc      = openObservationRPC(obsId, id, ks)
    val postRequest = POST(gdsUri, xmlRpc)

    // Do the request
    client
      .expect[Elem](postRequest)(scalaxml.xml)
      .attemptT
      .leftMap(handleConnectionError)
      .flatMap(handleXmlError)
  }

  // Build an xml rpc request to close an observation
  private def closeObservationRPC(id: ImageFileId): Elem =
    <methodCall>
      <methodName>HeaderReceiver.closeObservation</methodName>
      <params>
        <param>
          <value>
            <string>{id}</string>
          </value>
        </param>
      </params>
    </methodCall>

  def closeObservation(id: ImageFileId): SeqActionF[IO, Unit] = {
    // Build the request
    val xmlRpc      = closeObservationRPC(id)
    val postRequest = POST(gdsUri, xmlRpc)

    // Do the request
    client
      .expect[Elem](postRequest)(scalaxml.xml)
      .attemptT
      .leftMap(handleConnectionError)
      .flatMap(handleXmlError)
  }

  private def keywordsParam(ks: KeywordBag): Elem =
    <param>
      <value>
        <array>
          <data>
            {
              ks.keywords.map { k =>
                <value><string>{s"${k.name},${KeywordType.gdsKeywordType(k.keywordType)},${k.value}"}</string></value>
              }
            }
          </data>
        </array>
      </value>
    </param>
}

object GDSClient {

  def checkError(e: Elem, gdsUri: Uri): Either[SeqexecFailure, Unit] = {
    val v = for {
      m <- e \\ "methodResponse" \ "fault" \ "value" \ "struct" \\ "member"
      if (m \ "name").text === "faultString"
    } yield (m \ "value").text.trim
    v.headOption.fold(().asRight[SeqexecFailure])(
      SeqexecFailure.GDSXMLError(_, gdsUri).asLeft[Unit])
  }

  /**
    * Client for testing always returns ok
    */
  val alwaysOkClient: Client[IO] = {
    val service = HttpService[IO] {
      case _ =>
        val response =
          <methodResponse>
            <params>
              <param>
                  <value><string>Ok</string></value>
              </param>
            </params>
          </methodResponse>
        Response(Status.Ok).withBody(response)
    }
    Client.fromHttpService(service)
  }
}
