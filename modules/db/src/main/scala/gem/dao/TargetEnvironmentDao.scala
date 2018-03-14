// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._
import doobie.implicits._
import gem.enum.AsterismType
import gem.math.Index
import gem.syntax.treemap._
import scala.collection.immutable.{ TreeMap, TreeSet }

object TargetEnvironmentDao {

  def insert(oid: Observation.Id, e: TargetEnvironment): ConnectionIO[Unit] =
    for {
      _ <- e.asterism.fold(().pure[ConnectionIO])(AsterismDao.insert(oid, _))
      _ <- e.userTargets.toList.traverse(UserTargetDao.insert(oid, _)).void
    } yield ()

  def selectObs(oid: Observation.Id, at: Option[AsterismType]): ConnectionIO[TargetEnvironment] =
    for {
      a <- at.fold(Option.empty[Asterism].pure[ConnectionIO])(AsterismDao.select(oid, _))
      u <- UserTargetDao.selectObs(oid)
    } yield TargetEnvironment(a, u)

  def selectProg(pid: Program.Id, ats: Set[AsterismType]): ConnectionIO[Map[Index, TargetEnvironment]] =
    for {
      am <- ats.toList.traverse(AsterismDao.selectAll(pid, _)).map(ms => TreeMap.join(ms))
      um <- UserTargetDao.selectProg(pid)
    } yield am.mergeAll(um) {
      _.fold(a      => TargetEnvironment(Some(a), TreeSet.empty),
             u      => TargetEnvironment(None, u),
             (a, u) => TargetEnvironment(Some(a), u))
    }

}
