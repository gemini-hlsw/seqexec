// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.effect.Temporal
import org.http4s.client.Client
import org.http4s.client.dsl.Http4sClientDsl
import org.http4s.client.middleware.Retry
import org.http4s.client.middleware.RetryPolicy
import seqexec.model.Observation
import seqexec.model.dhs.ImageFileId

import scala.concurrent.duration._

/**
 * Gemini Data service client
 */
trait GdsClient[F[_]] extends Http4sClientDsl[F] {

  /**
   * Set the keywords for an image
   */
  def setKeywords(id: ImageFileId, ks: KeywordBag): F[Unit]

  def openObservation(obsId: Observation.Id, id: ImageFileId, ks: KeywordBag): F[Unit]

  def closeObservation(id: ImageFileId): F[Unit]

  def abortObservation(id: ImageFileId): F[Unit]

  def makeClient(base: Client[F])(implicit timer: Temporal[F]) = {
    val max             = 2
    var attemptsCounter = 1
    val policy          = RetryPolicy[F] { attempts: Int =>
      if (attempts >= max) None
      else {
        attemptsCounter = attemptsCounter + 1
        Some(10.milliseconds)
      }
    }
    Retry(policy)(base)
  }
}
