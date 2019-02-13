// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.dao.meta._
import gem.enum.{ GuideGroupType, Instrument }
import gem.enum.Guider._
import gem.enum.Instrument._
import gem.math.Index
import gem.syntax.treemap._
import gem.util.Zipper

import cats.implicits._

import doobie._
import doobie.implicits._

import scala.collection.immutable.TreeMap
import scala.reflect.runtime.universe._


final case class ProtoGuideGroup(
  gid:       GuideGroup.Id,
  pid:       Program.Id,
  inst:      Instrument,
  obsIndex:  Index,
  groupType: GuideGroupType,
  selected:  Boolean
)

object GuideEnvironmentDao {

  def selectObs(
    oid:  Observation.Id
  ): ConnectionIO[Option[GuideEnvironment]] =
    for {
      gs <- Statements.selectObs(oid).to[List]
      ss <- GuideStarDao.selectObs(oid)
    } yield guideEnvironment(gs, ss)

  def selectProg(
    pid: Program.Id
  ): ConnectionIO[TreeMap[Index, Option[GuideEnvironment]]] =
    for {
      gs <- Statements.selectProg(pid).to[List]
      ss <- GuideStarDao.selectProg(pid)
    } yield TreeMap.grouping(gs)(_.obsIndex).mergeMatchingKeys(ss) { (obsGroups, obsStars) =>
      obsStars.flatMap(guideEnvironment(obsGroups, _))
    }


  // Assemble all the groups and guide stars into an actual GuideEnvironment
  private object guideEnvironment {
    type GroupResolver[T] = PartialFunction[List[ResolvedGuideStar], T]

    val flamingos2: GroupResolver[GuideGroup.Flamingos2] = {
      case List(ResolvedGuideStar(t, F2OI, Flamingos2)) => GuideGroup.Flamingos2.OI(t)
      case List(ResolvedGuideStar(t, P1GS, Flamingos2)) => GuideGroup.Flamingos2.P1(t)
      case List(ResolvedGuideStar(t, P2GS, Flamingos2)) => GuideGroup.Flamingos2.P2(t)
    }

    val gmosNorth: GroupResolver[GuideGroup.GmosNorth] = {
      case List(ResolvedGuideStar(t, GmosNOI, GmosN)) => GuideGroup.GmosNorth.OI(t)
      case List(ResolvedGuideStar(t, P1GN,    GmosN)) => GuideGroup.GmosNorth.P1(t)
      case List(ResolvedGuideStar(t, P2GN,    GmosN)) => GuideGroup.GmosNorth.P2(t)
    }

    val gmosSouth: GroupResolver[GuideGroup.GmosSouth] = {
      case List(ResolvedGuideStar(t, GmosSOI, GmosS)) => GuideGroup.GmosSouth.OI(t)
      case List(ResolvedGuideStar(t, P1GS,    GmosS)) => GuideGroup.GmosSouth.P1(t)
      case List(ResolvedGuideStar(t, P2GS,    GmosS)) => GuideGroup.GmosSouth.P2(t)
    }

    // Partition the manual guide groups into Left[GuideGroup.Id] if none is
    // select or else Zipper[GuideGroup.Id] if there is a selection.
    def manual(ps: List[ProtoGuideGroup]): Either[List[GuideGroup.Id], Zipper[GuideGroup.Id]] =
      ps.span(!_.selected) match {
        case (lefts, focus :: rights) => Zipper(lefts.reverse, focus, rights).map(_.gid).asRight
        case (all,   Nil            ) => all.map(_.gid).asLeft
      }

    // Shape the group list into a GuideOptions[GuideGroup.Id] according to
    // which is auto (if any) vs. manual and which is selected (if any).  This
    // will have the right GuideOptions structure, but the elements will be
    // GuideOptions[GuideGroup.Id] instead of the typed GuideGroup.{instrument}.
    def shape(ps: List[ProtoGuideGroup]): GuideOptions[GuideGroup.Id] =
      ps.partition(_.groupType === GuideGroupType.Automatic) match {
        case (Nil,     ms) => GuideOptions(None,        manual(ms))
        case (List(a), ms) => GuideOptions(Some(a.gid), manual(ms))
        case (as,       _) =>
          sys.error(s"Multiple auto guide groups: $as")
      }

    def instrument(protos: List[ProtoGuideGroup]): Option[Instrument] =
      protos.map(_.inst).distinct match {
        case Nil     => None
        case List(i) => Some(i)
        case x       =>
          sys.error(s"Cannot create a guide environment for multiple instruments: $x")
      }

    def apply(
      protos: List[ProtoGuideGroup],
      stars:  Map[GuideGroup.Id, List[ResolvedGuideStar]]
    ): Option[GuideEnvironment] = {

      def options[T: TypeTag](gr: GroupResolver[T]): GuideOptions[T] = {
        val groups: Map[GuideGroup.Id, T] = stars.mapValues { s =>
          gr.applyOrElse(s, (_: List[ResolvedGuideStar]) => sys.error(s"Could not create a group of type ${typeOf[T]} from $s"))
        }

        shape(protos).map { gid =>
          groups.getOrElse(gid, sys.error(s"Missing guide group with id $gid"))
        }
      }

      instrument(protos).map {
        case Flamingos2 => GuideEnvironment.Flamingos2(options(flamingos2))
        case GmosN      => GuideEnvironment.GmosNorth(options(gmosNorth))
        case GmosS      => GuideEnvironment.GmosSouth(options(gmosSouth))
        case inst       =>
          sys.error(s"Cannot create a guide environment for $inst")
      }
    }

  }


  object Statements {

    import EnumeratedMeta._
    import IndexMeta._
    import ProgramIdMeta._

    private val selectGuideGroup: Fragment =
      Fragment.const(
        """
          SELECT id,
                 program_id,
                 observation_index,
                 instrument,
                 type,
                 selected
            FROM guide_group
        """
      )

    private def selectWhere(where: Fragment): Query0[ProtoGuideGroup] =
      (selectGuideGroup ++ where).query[ProtoGuideGroup]

    def selectObs(oid: Observation.Id): Query0[ProtoGuideGroup] =
      selectWhere(fr"WHERE program_id = ${oid.pid} AND observation_index = ${oid.index}")

    def selectProg(pid: Program.Id): Query0[ProtoGuideGroup] =
      selectWhere(fr"WHERE program_id = $pid")

  }

}
