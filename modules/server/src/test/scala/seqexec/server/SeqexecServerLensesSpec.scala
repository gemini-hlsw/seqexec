// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.Eq
import cats.tests.CatsSuite
import edu.gemini.spModel.config2.{ Config, ItemKey }
import seqexec.model.arb.ArbObservationId
import seqexec.model.Observation
import monocle.law.discipline.LensTests
import seqexec.model.enum.Instrument
import seqexec.model.SystemOverrides
import seqexec.engine
import seqexec.engine.{ Action, ParallelActions }
import SequenceGen._
import cats.effect.IO

/**
 * Tests SeqexecServer Lenses
 */
final class SeqexecServerLensesSpec
    extends CatsSuite
    with SeqexecServerArbitraries
    with ArbObservationId {

  implicit val eqItemKeys: Eq[Map[ItemKey, AnyRef]] = Eq.fromUniversalEquals
  implicit val eqLegacyConfig: Eq[Config]           = Eq.fromUniversalEquals
  implicit val eqCleanConfig: Eq[CleanConfig]       =
    Eq.by(x => (x.config, x.overrides))

  // I tried to go down the rabbit hole with the Eqs, but it is not worth it for what they are used.
  implicit def actStateEq[F[_]]: Eq[Action.State[F]]                                             = Eq.fromUniversalEquals
  implicit def actionEq[F[_]]: Eq[Action[F]]                                                     = Eq.by(x => (x.kind, x.state))
  // Formally, to probe equality between two functions it must be probed that they produce the same result for all
  // possible inputs. We settle for the `UniversalEquals` instead.
  implicit def steplEq[F[_]]: Eq[HeaderExtraData => List[ParallelActions[F]]]                    =
    Eq.fromUniversalEquals
  implicit def stepmEq[F[_]]: Eq[(HeaderExtraData, SystemOverrides) => List[ParallelActions[F]]] =
    Eq.fromUniversalEquals
  implicit def stepnEq[F[_]]: Eq[SystemOverrides => Action[F]]                                   = Eq.fromUniversalEquals
  implicit def stepActionsGenEq[F[_]]: Eq[StepActionsGen[F]]                                     = Eq.by(x => (x.configs, x.post))
  implicit def pndstepgEq[F[_]]: Eq[PendingStepGen[F]]                                           =
    Eq.by(x => (x.id, x.config, x.resources, x.generator))
  implicit val skipstepgEq: Eq[SkippedStepGen]                                                   = Eq.by(x => (x.id, x.config))
  implicit val cmpstepgEq: Eq[CompletedStepGen]                                                  = Eq.by(x => (x.id, x.config, x.fileId))
  implicit def stepqEq[F[_]]: Eq[StepGen[F]]                                                     = Eq.instance {
    case (a: PendingStepGen[F], b: PendingStepGen[F]) => a === b
    case (a: SkippedStepGen, b: SkippedStepGen)       => a === b
    case (a: CompletedStepGen, b: CompletedStepGen)   => a === b
    case _                                            => false
  }
  implicit def seqgEq[F[_]]: Eq[SequenceGen[F]]                                                  = Eq.by(x => (x.id, x.title, x.instrument, x.steps))
  implicit def obsseqEq[F[_]]: Eq[SequenceData[F]]                                               = Eq.by(x => (x.observer, x.seqGen))
  implicit def seqstateEq[F[_]]: Eq[engine.Sequence.State[F]]                                    = Eq.fromUniversalEquals
  implicit val stateEq: Eq[EngineState[IO]]                                                      =
    Eq.by(x => (x.queues, x.selected, x.conditions, x.operator, x.sequences))

  checkAll("selected optional", LensTests(EngineState.instrumentLoadedL[IO](Instrument.Gpi)))

  private val seqId = Observation.Id.unsafeFromString("GS-2018B-Q-0-1")
  // Some sanity checks
  test("Support inserting new loaded sequences") {
    val base = EngineState
      .default[IO]
      .copy(selected = Map(Instrument.F2 -> Observation.Id.unsafeFromString("GS-2018B-Q-1-1")))
    EngineState
      .instrumentLoadedL(Instrument.Gpi)
      .replace(seqId.some)
      .apply(base) shouldEqual base.copy(selected = base.selected + (Instrument.Gpi -> seqId))
  }
  test("Support replacing loaded sequences") {
    val base = EngineState
      .default[IO]
      .copy(
        selected = Map(Instrument.Gpi -> Observation.Id.unsafeFromString("GS-2018B-Q-1-1"),
                       Instrument.F2  -> Observation.Id.unsafeFromString("GS-2018B-Q-2-1")
        )
      )
    EngineState
      .instrumentLoadedL(Instrument.Gpi)
      .replace(seqId.some)
      .apply(base) shouldEqual base.copy(selected = base.selected.updated(Instrument.Gpi, seqId))
  }
}
