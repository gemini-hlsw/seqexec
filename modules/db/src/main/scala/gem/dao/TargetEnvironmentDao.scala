// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package dao

import gem.syntax.treesetcompanion._
import cats.implicits._

import doobie._
import doobie.implicits._

import scala.collection.immutable.TreeSet


// At the moment, TargetEnvironment just wraps user targets but it will grow to
// encompass the science asterism and guide targets as well.

object TargetEnvironmentDao {

  def insert(oid: Observation.Id, e: TargetEnvironment): ConnectionIO[Unit] =
    e.userTargets.toList.traverse(UserTargetDao.insert(oid, _)).void

  def select(oid: Observation.Id): ConnectionIO[TargetEnvironment] =
    UserTargetDao.selectAll(oid: Observation.Id).map { lst =>
      TargetEnvironment(TreeSet.fromList(lst.unzip._2))
    }

}
