// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.engine.Result.RetVal
import seqexec.model.dhs.ImageFileId
import seqexec.model.enum.Resource

sealed trait Response extends RetVal with Product with Serializable

object Response {

  final case class Configured(resource: Resource) extends Response

  final case class Observed(fileId: ImageFileId) extends Response

  final case class Aborted(fileId: ImageFileId) extends Response

  case object Ignored extends Response

}
