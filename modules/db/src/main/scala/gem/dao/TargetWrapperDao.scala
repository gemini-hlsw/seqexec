// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.syntax.treemap._

import cats.implicits._
import doobie._
import doobie.implicits._

import scala.collection.immutable.TreeMap

/**
 * A proto-target wrapper. A target wrapper refers to a class that wraps a
 * Target instance and adds some additional data such as an associated category
 * of target or guide probe meant to observe the target.  A proto-target
 * wrapper collects data used for grouping target wrappers in various ways.
 * The target wrapper itself can be instantiated by providing the target to
 * wrap.
 *
 * For example, `UserTarget` wraps a target and adds a `UserTargetType` to
 * differentiate user targets that are blind offsets from those that are tuning
 * stars, etc.  A `GuideTarget` wraps a target and adds a guide probe that
 * should be used to observe it.
 *
 * @tparam I target wrapper id type
 * @tparam W target wrapper type
 */
trait ProtoTargetWrapper[I, W] {
  def id: I
  def targetId: Target.Id

  /** Instantiates the target wrapper with the supplied Target instance. */
  def wrap(t: Target): W
}

/**
 * Support for selecting target wrappers and grouping them in various ways. For
 * instance, all the user targets for a particular observation.
 *
 * @tparam I id type of the target wrapper
 * @tparam W target wrapper type
 * @tparam P proto-target wrapper type
 */
trait TargetWrapperDao[I, W, P <: ProtoTargetWrapper[I, W]] {

  // A selection of proto target wrappers the corresponding target.
  protected type Selection = List[(P, Target)]

  /**
   * Selects a single proto target wrapper and instantiates it to a target
   * wrapper.
   */
  protected def selectOne(query: Query0[P]): ConnectionIO[Option[W]] = {
    def lookupAndWrap(p: P): ConnectionIO[Option[W]] =
      TargetDao.select(p.targetId).map(_.map(p.wrap))

    for {
      p <- query.option
      w <- p.fold(Option.empty[W].pure[ConnectionIO])(lookupAndWrap)
    } yield w
  }

  /**
   * Selects all the target wrappers according to the query and pairs them with
   * the corresponding target.
   */
  protected def selectAll(query: Query0[P]): ConnectionIO[Selection] =
    for {
      ps <- query.to[List]
      ts <- ps.map(_.targetId).traverse(TargetDao.select)
    } yield ps.zip(ts).flatMap { case (p, ot) =>
      ot.map(t => (p, t)).toList
    }

  /** Groups a selection according to its unique id. */
  protected def groupById(sel: Selection)(implicit ev: Ordering[I]): TreeMap[I, W] =
    sel.foldLeft(TreeMap.empty[I, W]) { case (m, (p, t)) =>
      m.updated(p.id, p.wrap(t))
    }

  /**
   * Groups a selection according to a grouping function then maps each group
   * according to a mapping function.
   *
   * @param gf grouping function
   * @param mf mapping function
   * @param sel selection of proto-target wrapper and target
   *
   * @tparam A key type of the grouping
   * @tparam B type of the mapped values
   */
  protected def groupAndMap[A: Ordering, B](
    gf: P         => A,
    mf: Selection => B
  )(sel: Selection): TreeMap[A, B] =
    TreeMap.grouping(sel) { case (p, _) => gf(p) }
           .treeMapValues(mf)

  /**
   * Instantiates the target wrapper for each element of the selection. This
   * is expected to be the last step after any groupings by the information
   * kept in the proto-target wrapper have been performed.
   */
  protected def wrapAll(sel: Selection): List[W] =
    sel.map { case (p, t) => p.wrap(t) }
}