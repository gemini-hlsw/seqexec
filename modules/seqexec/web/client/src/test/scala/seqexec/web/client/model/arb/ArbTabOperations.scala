// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.arb

import org.scalacheck.Arbitrary._
import org.scalacheck._
import scala.collection.immutable.SortedMap
import gem.arb.ArbEnumerated._
import seqexec.model.enum.Resource
import seqexec.web.client.model._
import seqexec.web.client.model.RunOperation

trait ArbTabOperations {
  implicit val arbResourceRunOperation: Arbitrary[ResourceRunOperation] = {
    Arbitrary {
      for {
        i <- arbitrary[Int]
        s <- Gen.oneOf(ResourceRunOperation.ResourceRunIdle,
                       ResourceRunOperation.ResourceRunInFlight(i),
                       ResourceRunOperation.ResourceRunCompleted(i))
      } yield s
    }
  }

  implicit val rroCogen: Cogen[ResourceRunOperation] =
    Cogen[Option[Either[Int, Either[Int, Int]]]].contramap {
      case ResourceRunOperation.ResourceRunIdle         => None
      case ResourceRunOperation.ResourceRunInFlight(i)  => Some(Left(i))
      case ResourceRunOperation.ResourceRunCompleted(i) => Some(Right(Right(i)))
      case ResourceRunOperation.ResourceRunFailed(i)    => Some(Right(Left(i)))
    }

  implicit val arbTabOperations: Arbitrary[TabOperations] = {
    implicit val ordering: Ordering[Resource] =
      cats.Order[Resource].toOrdering

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
  }

  implicit val toCogen: Cogen[TabOperations] = {
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
         x.resourceRunRequested.toList)
    )
  }

}

object ArbTabOperations extends ArbTabOperations
