// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats._
import cats.implicits._
import doobie._
import doobie.implicits._
import gem.instances.treeset._
import gem.math.Index

object TargetEnvironmentDao {

  private implicit def m: Monoid[ConnectionIO[Unit]] =
    Applicative.monoid

  def insert(oid: Observation.Id, e: TargetEnvironment): ConnectionIO[Unit] =
    e.asterism.foldMap(AsterismDao.insert(oid, _)) *>
    e.userTargets.toList.traverse(UserTargetDao.insert(oid, _)).void

  def selectObs(oid: Observation.Id): ConnectionIO[TargetEnvironment] =
      (AsterismDao.select(oid), GuideEnvironmentDao.selectObs(oid), UserTargetDao.selectObs(oid)).mapN {
        case (Left(a), genv, uts)  => TargetEnvironment.fromAsterism(a, genv, uts)
        case (Right(i), genv, uts) => TargetEnvironment.fromInstrument(i, genv, uts)
    }

  def selectProg(pid: Program.Id): ConnectionIO[Map[Index, TargetEnvironment]] =
    (AsterismDao.selectAll(pid), GuideEnvironmentDao.selectProg(pid), UserTargetDao.selectProg(pid)).mapN { (am, gm, um) =>
      am.map { case (idx, e) =>
        val g = gm.get(idx).flatten
        val u = um.get(idx).orEmpty
        e match {
          case Left(a)  => idx -> TargetEnvironment.fromAsterism(a, g, u)
          case Right(i) => idx -> TargetEnvironment.fromInstrument(i, g, u)
        }
      }
    }

}
