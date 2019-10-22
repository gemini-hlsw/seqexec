// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.enum

import gem.util.Enumerated

sealed trait ApplyCommandResult extends Product with Serializable

object ApplyCommandResult {
  case object Paused extends ApplyCommandResult
  case object Completed extends ApplyCommandResult

  /** @group Typeclass Instances */
  implicit val ApplyCommandResultEnumerated: Enumerated[ApplyCommandResult] =
    Enumerated.of(Paused, Completed)
}
