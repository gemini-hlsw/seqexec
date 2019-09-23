// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats._
import cats.implicits._
import doobie._
import doobie.implicits._
import gem.instances.treeset._
import gsp.math.Index

object TargetEnvironmentDao {

  private implicit def m: Monoid[ConnectionIO[Unit]] =
    Applicative.monoid

  def insert(oid: Observation.Id, e: TargetEnvironment): ConnectionIO[Unit] =
    e.asterism.foldMap(AsterismDao.insert(oid, _)) *>
    e.userTargets.toList.traverse_(UserTargetDao.insert(oid, _))

  def selectObs(oid: Observation.Id): ConnectionIO[TargetEnvironment] =
    (AsterismDao.select(oid), UserTargetDao.selectObs(oid)).mapN {
      case (Left(a), uts)  => TargetEnvironment.fromAsterism(a, uts)
      case (Right(i), uts) => TargetEnvironment.fromInstrument(i, uts)
    }

  def selectProg(pid: Program.Id): ConnectionIO[Map[Index, TargetEnvironment]] =
    (AsterismDao.selectAll(pid), UserTargetDao.selectProg(pid)).mapN { (am, um) =>
      am.map { case (idx, e) =>
        val uts = um.get(idx).orEmpty
        e match {
          case Left(a)  => idx -> TargetEnvironment.fromAsterism(a, uts)
          case Right(i) => idx -> TargetEnvironment.fromInstrument(i, uts)
        }
      }
    }

}
