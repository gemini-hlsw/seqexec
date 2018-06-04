// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import cats.implicits._
import doobie._
import doobie.implicits._
import gem.enum.{ AsterismType, Instrument }
import gem.math.Index
import gem.syntax.treemap._
import scala.collection.immutable.{ TreeMap, TreeSet }

object TargetEnvironmentDao {

  def insert(oid: Observation.Id, e: TargetEnvironment): ConnectionIO[Unit] =
    for {
      _ <- e.asterism.fold(().pure[ConnectionIO])(AsterismDao.insert(oid, _))
      _ <- e.userTargets.toList.traverse(UserTargetDao.insert(oid, _)).void
    } yield ()

  // N.B. the instrument is only used for the empty case, which is brittle. It may be inconsistent
  // with the instrument in the non-empty case (should be impossible, heh-heh).
  def selectObs(oid: Observation.Id, at: Option[AsterismType], i: Instrument): ConnectionIO[TargetEnvironment] =
    for {
      a <- at.fold(Option.empty[Asterism].pure[ConnectionIO])(AsterismDao.select(oid, _))
      u <- UserTargetDao.selectObs(oid)
    } yield a.fold(targetEnvironmentWithoutAsterism(i, u))(TargetEnvironment.fromAsterism(_, u))

  def selectProg(pid: Program.Id, ats: Set[AsterismType], i: Instrument): ConnectionIO[Map[Index, TargetEnvironment]] =
    for {
      am <- ats.toList.traverse(AsterismDao.selectAll(pid, _)).map(ms => TreeMap.join(ms))
      um <- UserTargetDao.selectProg(pid)
    } yield am.mergeAll(um) {
      _.fold(a      => TargetEnvironment.fromAsterism(a, TreeSet.empty),
             u      => targetEnvironmentWithoutAsterism(i, u),
             (a, u) => TargetEnvironment.fromAsterism(a, u))
    }

  private def targetEnvironmentWithoutAsterism(i: Instrument, ts: TreeSet[UserTarget]): TargetEnvironment =
    i match {
      case Instrument.Phoenix    => TargetEnvironment.Phoenix(None, ts)
      case Instrument.Michelle   => TargetEnvironment.Michelle(None, ts)
      case Instrument.Gnirs      => TargetEnvironment.Gnirs(None, ts)
      case Instrument.Niri       => TargetEnvironment.Niri(None, ts)
      case Instrument.Trecs      => TargetEnvironment.Trecs(None, ts)
      case Instrument.Nici       => TargetEnvironment.Nici(None, ts)
      case Instrument.Nifs       => TargetEnvironment.Nifs(None, ts)
      case Instrument.Gpi        => TargetEnvironment.Gpi(None, ts)
      case Instrument.Gsaoi      => TargetEnvironment.Gsaoi(None, ts)
      case Instrument.GmosS      => TargetEnvironment.GmosS(None, ts)
      case Instrument.AcqCam     => TargetEnvironment.AcqCam(None, ts)
      case Instrument.GmosN      => TargetEnvironment.GmosN(None, ts)
      case Instrument.Bhros      => TargetEnvironment.Bhros(None, ts)
      case Instrument.Visitor    => TargetEnvironment.Visitor(None, ts)
      case Instrument.Flamingos2 => TargetEnvironment.Flamingos2(None, ts)
      case Instrument.Ghost      => TargetEnvironment.Ghost(None, ts)
    }

}
