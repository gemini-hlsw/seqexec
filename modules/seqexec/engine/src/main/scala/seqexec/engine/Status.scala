// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

/**
  * Flag to indicate whether the global execution is `Running` or `Waiting`.
  */
sealed trait Status

object Status {
  case object Waiting   extends Status
  case object Completed extends Status
  case object Running   extends Status
}
