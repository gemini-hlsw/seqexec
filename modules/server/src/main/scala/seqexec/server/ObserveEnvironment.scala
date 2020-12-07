// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.model.Observation
import seqexec.model.dhs.DataId
import seqexec.server.keywords._
import seqexec.server.tcs.Tcs

/**
 * Describes the parameters for an observation
 */
final case class ObserveEnvironment[F[_]](
  odb:      OdbProxy[F],
  dhs:      DhsClient[F],
  config:   CleanConfig,
  stepType: StepType,
  obsId:    Observation.Id,
  dataId:   DataId,
  inst:     InstrumentSystem[F],
  insSpecs: InstrumentSpecifics,
  otherSys: List[System[F]],
  headers:  HeaderExtraData => List[Header[F]],
  ctx:      HeaderExtraData
) {
  def getTcs: Option[Tcs[F]] = otherSys.collectFirst { case x: Tcs[F] => x }
}
