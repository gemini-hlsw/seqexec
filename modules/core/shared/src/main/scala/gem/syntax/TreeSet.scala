// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.syntax

import scala.collection.immutable.TreeSet

final class TreeSetCompanionOps(val self: TreeSet.type) extends AnyVal {

  /** Creates a `TreeSet` from a `List`, provided an `Ordering` is available. */
  def fromList[A: Ordering](lst: List[A]): TreeSet[A] =
    TreeSet(lst: _*)

}

trait ToTreeSetCompanionOps {
  implicit def ToTreeSetCompanionOps(c: TreeSet.type): TreeSetCompanionOps =
    new TreeSetCompanionOps(c)
}

object treesetcompanion extends ToTreeSetCompanionOps