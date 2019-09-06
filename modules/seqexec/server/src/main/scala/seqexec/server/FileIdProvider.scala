// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.model.dhs.ImageFileId
import seqexec.server.keywords._

object FileIdProvider {

  def fileId[F[_]](env: ObserveEnvironment[F]): F[ImageFileId] =
    // All instruments ask the DHS for an ImageFileId
    env.systems.dhs.createImage(
      DhsClient.ImageParameters(DhsClient.Permanent,
                                List(env.inst.contributorName, "dhs-http"))
    )

}
