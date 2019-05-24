// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.arb

import org.scalacheck.Arbitrary._
import org.scalacheck._
import scala.collection.immutable.SortedMap
import seqexec.model.enum.Resource
import seqexec.model.SeqexecModelArbitraries._
import seqexec.web.client.model._
import seqexec.web.client.model.RunOperation

trait ArbTabOperations {
  implicit val arbRunOperation: Arbitrary[RunOperation] =
    Arbitrary(Gen.oneOf(RunOperation.RunIdle, RunOperation.RunInFlight))

  implicit val roCogen: Cogen[RunOperation] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbSyncOperation: Arbitrary[SyncOperation] =
    Arbitrary(Gen.oneOf(SyncOperation.SyncIdle, SyncOperation.SyncInFlight))

  implicit val soCogen: Cogen[SyncOperation] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbPauseOperation: Arbitrary[PauseOperation] =
    Arbitrary(Gen.oneOf(PauseOperation.PauseIdle, PauseOperation.PauseInFlight))

  implicit val poCogen: Cogen[PauseOperation] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbStopOperation: Arbitrary[StopOperation] =
    Arbitrary(Gen.oneOf(StopOperation.StopIdle, StopOperation.StopInFlight))

  implicit val stoCogen: Cogen[StopOperation] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbAbortOperation: Arbitrary[AbortOperation] =
    Arbitrary(Gen.oneOf(AbortOperation.AbortIdle, AbortOperation.AbortInFlight))

  implicit val abtCogen: Cogen[AbortOperation] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbResumeOperation: Arbitrary[ResumeOperation] =
    Arbitrary(Gen.oneOf(ResumeOperation.ResumeIdle, ResumeOperation.ResumeInFlight))

  implicit val resCogen: Cogen[ResumeOperation] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbResourceRunOperation: Arbitrary[ResourceRunOperation] =
    Arbitrary(
      Gen.oneOf(ResourceRunOperation.ResourceRunIdle,
                ResourceRunOperation.ResourceRunInFlight,
                ResourceRunOperation.ResourceRunCompleted))

  implicit val rruCogen: Cogen[ResourceRunOperation] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbStartFromOperation: Arbitrary[StartFromOperation] =
    Arbitrary(
      Gen.oneOf(StartFromOperation.StartFromIdle,
                StartFromOperation.StartFromInFlight))

  implicit val sfoCogen: Cogen[StartFromOperation] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbCancelPauseOperation: Arbitrary[CancelPauseOperation] =
    Arbitrary(
      Gen.oneOf(CancelPauseOperation.CancelPauseIdle,
                CancelPauseOperation.CancelPauseInFlight))

  implicit val cpuCogen: Cogen[CancelPauseOperation] =
    Cogen[String].contramap(_.productPrefix)

  implicit val arbTabOperations: Arbitrary[TabOperations] =
    Arbitrary {
      for {
        r <- arbitrary[RunOperation]
        s <- arbitrary[SyncOperation]
        p <- arbitrary[PauseOperation]
        c <- arbitrary[CancelPauseOperation]
        m <- arbitrary[ResumeOperation]
        t <- arbitrary[StopOperation]
        a <- arbitrary[AbortOperation]
        f <- arbitrary[StartFromOperation]
        u <- arbitrary[SortedMap[Resource, ResourceRunOperation]]
      } yield TabOperations(r, s, p, c, m, t, a, f, u)
    }

  implicit val toCogen: Cogen[TabOperations] = {
    implicit val rrc = seqexec.model.SeqexecModelArbitraries.resCogen
    Cogen[(RunOperation,
           SyncOperation,
           PauseOperation,
           CancelPauseOperation,
           ResumeOperation,
           StopOperation,
           AbortOperation,
           StartFromOperation,
           List[(Resource, ResourceRunOperation)])].contramap(
      x =>
        (x.runRequested,
         x.syncRequested,
         x.pauseRequested,
         x.cancelPauseRequested,
         x.resumeRequested,
         x.stopRequested,
         x.abortRequested,
         x.startFromRequested,
         x.resourceRunRequested.toList))
  }

}

object ArbTabOperations extends ArbTabOperations
