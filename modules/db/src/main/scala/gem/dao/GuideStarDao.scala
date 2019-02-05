// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.dao.meta._
import gem.enum.{ Guider, Instrument }
import gem.math.Index

import cats.implicits._

import doobie._
import doobie.implicits._

import scala.collection.immutable.TreeMap


final case class ProtoGuideStar(
  id:       GuideStar.Id,
  groupId:  GuideGroup.Id,
  targetId: Target.Id,
  guider:   Guider,
  obsIndex: Index
) extends ProtoTargetWrapper[GuideStar.Id, (Target, Guider)] {

  override def wrap(t: Target): (Target, Guider) =
    (t, guider)

}

object GuideStarDao extends TargetWrapperDao[GuideStar.Id, (Target, Guider), ProtoGuideStar] {

  def insert(
    gid:        GuideGroup.Id,
    target:     Target,
    guider:     Guider,
    oid:        Observation.Id,
    instrument: Instrument
  ): ConnectionIO[GuideStar.Id] =
    for {
      t <- TargetDao.insert(target)
      i <- Statements.insert(gid, t, guider, oid, instrument)
                     .withUniqueGeneratedKeys[Int]("id")
                     .map(GuideStar.Id(_))
    } yield i


  def select(
    id: GuideStar.Id
  ): ConnectionIO[Option[(Target, Guider)]] =
    selectOne(Statements.select(id))

  def selectGroup(
    gid: GuideGroup.Id
  ): ConnectionIO[List[(Target, Guider)]] =
    selectAll(Statements.selectGroup(gid))
      .map(wrapAll)

  def selectGroupWithId(
    gid: GuideGroup.Id
  ): ConnectionIO[TreeMap[GuideStar.Id, (Target, Guider)]] =
    selectAll(Statements.selectGroup(gid))
      .map(groupById)

  def selectObs(
    oid: Observation.Id
  ): ConnectionIO[TreeMap[GuideGroup.Id, List[(Target, Guider)]]] =
    selectAll(Statements.selectObs(oid))
      .map(groupAndMap(_.groupId, wrapAll))

  def selectObsWithId(
    oid: Observation.Id
  ): ConnectionIO[TreeMap[GuideGroup.Id, TreeMap[GuideStar.Id, (Target, Guider)]]] =
    selectAll(Statements.selectObs(oid))
      .map(groupAndMap(_.groupId, groupById))

  def selectProg(
    pid: Program.Id
  ): ConnectionIO[TreeMap[Index, TreeMap[GuideGroup.Id, List[(Target, Guider)]]]] =
    selectAll(Statements.selectProg(pid))
      .map(groupAndMap(_.obsIndex, groupAndMap(_.groupId, wrapAll)))

  def selectProgWithId(
    pid: Program.Id
  ): ConnectionIO[TreeMap[Index, TreeMap[GuideGroup.Id, TreeMap[GuideStar.Id, (Target, Guider)]]]] =
    selectAll(Statements.selectProg(pid))
      .map(groupAndMap(_.obsIndex, groupAndMap(_.groupId, groupById)))

  object Statements {

    import EnumeratedMeta._

    import gem.dao.meta.ProgramIdMeta._
    import gem.dao.meta.IndexMeta._

    def insert(
      gid:        GuideGroup.Id,
      tid:        Target.Id,
      guider:     Guider,
      oid:        Observation.Id,
      instrument: Instrument
    ): Update0 =
      sql"""
        INSERT INTO guide_star (
          group_id,
          target_id,
          guider,
          guider_instrument,
          program_id,
          observation_index,
          instrument
        ) VALUES (
          $gid,
          $tid,
          $guider,
          ${guider.instrument},
          ${oid.pid},
          ${oid.index},
          $instrument
        )
      """.update

    private val selectGuideStar: Fragment =
      Fragment.const(
        """
           SELECT id,
                  group_id,
                  target_id,
                  guider,
                  observation_index
             FROM guide_star
        """
      )

    private def selectWhere(where: Fragment): Query0[ProtoGuideStar] =
      (selectGuideStar ++ where).query[ProtoGuideStar]

    def select(id: GuideStar.Id): Query0[ProtoGuideStar] =
      selectWhere(fr"WHERE id = $id")

    def selectGroup(gid: GuideGroup.Id): Query0[ProtoGuideStar] =
      selectWhere(fr"WHERE group_id = $gid")

    def selectObs(oid: Observation.Id): Query0[ProtoGuideStar] =
      selectWhere(fr"WHERE program_id = ${oid.pid} AND observation_index = ${oid.index}")

    def selectProg(pid: Program.Id): Query0[ProtoGuideStar] =
      selectWhere(fr"WHERE program_id = $pid")
  }

}
