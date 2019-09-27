// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gmos

import cats.implicits._
import seqexec.model._
import seqexec.model.enum._
import seqexec.engine
import seqexec.server._
import seqexec.server.gmos.GmosController.Config._

final class GmosStepsView[F[_]] extends StepsView[F] {
  def stepView(
    stepg:        SequenceGen.StepGen[F],
    step:         engine.Step[F],
    altCfgStatus: List[(Resource, ActionStatus)]
  ): Step =
    Gmos.nsConfig(stepg.config) match {
      case Right(e @ NSConfig.NodAndShuffle(c, _, _, _)) =>
        val status = engine.Step.status(step)
        val configStatus =
          if (StepsView.runningOrComplete(status)) {
            StepsView.stepConfigStatus(step)
          } else {
            altCfgStatus
          }

        NodAndShuffleStep(
          id           = step.id,
          config       = stepg.config.toStepConfig,
          status       = status,
          breakpoint   = step.breakpoint.self,
          skip         = step.skipMark.self,
          configStatus = configStatus,
          nsStatus =
            NodAndShuffleStatus(StepsView.observeStatus(step.executions),
                                e.totalExposureTime,
                                e.nodExposureTime,
                                c),
          fileId = StepsView
            .fileId(step.executions)
            .orElse(stepg.some.collect {
              case SequenceGen.CompletedStepGen(_, _, fileId) => fileId
            }.flatten)
        )
      case _ =>
        StepsView.defaultStepsView.stepView(stepg, step, altCfgStatus)
    }

}

object GmosStepsView {
  def stepsView[F[_]]: StepsView[F] = new GmosStepsView[F]
}
