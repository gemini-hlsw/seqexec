// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import seqexec.model.dhs.ImageFileId

/**
  * Defines the interface for dhs client, with methods, e.g. to request image creation
  */
trait DhsClient[F[_]] {
  /**
    * Requests the DHS to create an image returning the obs id if applicable
    */
  def createImage(p: DhsClient.ImageParameters): F[ImageFileId]

  /**
    * Set the keywords for an image
    */
  def setKeywords(id: ImageFileId, keywords: KeywordBag, finalFlag: Boolean): F[Unit]

}

object DhsClient {

  type Contributor = String

  sealed case class Lifetime(str: String)
  object Permanent extends Lifetime("PERMANENT")
  object Temporary extends Lifetime("TEMPORARY")
  object Transient extends Lifetime("TRANSIENT")

  final case class ImageParameters(lifetime: Lifetime, contributors: List[Contributor])

}
