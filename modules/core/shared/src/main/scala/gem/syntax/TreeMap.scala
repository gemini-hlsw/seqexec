// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.syntax

import cats.Foldable
import cats.data.Ior

import scala.collection.immutable.TreeMap

final class TreeMapCompanionOps(val self: TreeMap.type) extends AnyVal {

  /**
   * Creates a `TreeMap` from a `List[(A, B)]`, provided an `Ordering[A]`
   * is available.
   */
  def fromList[A: Ordering, B](lst: List[(A, B)]): TreeMap[A, B] =
    TreeMap(lst: _*)

  /**
   * Creates a `TreeMap` from a `Foldable[(A, B)]`, provided an `Ordering[A]`
   * is available.
   */
  def fromFoldable[F[_], A, B](fab: F[(A, B)])(implicit F: Foldable[F], A: Ordering[A]): TreeMap[A, B] =
    fromList(F.toList(fab))

  /**
   * Combines all the given maps into a single map, where keys common to two or
   * more maps are the value of the last occurrence in the list.
   */
  def join[A: Ordering, B](ms: List[TreeMap[A, B]]): TreeMap[A, B] =
    ms.foldLeft(TreeMap.empty[A, B]) { case (m0, m1) => m0 ++ m1 }
}

trait ToTreeMapCompanionOps {
  implicit def ToTreeMapCompanionOps(c: TreeMap.type): TreeMapCompanionOps =
    new TreeMapCompanionOps(c)
}

final class TreeMapOps[A, B](val self: TreeMap[A, B]) extends AnyVal {

  /**
   * Combines this map with the values of matching keys from `that` map using
   * supplied function.  Keys that only exist in `that` are ignored, but the
   * value of keys that exist in this map but not in `that` map are passed to
   * the function as None.
   *
   * @param that the map to combine with this one
   * @param f function that combines the values
   */
  def mergeMatchingKeys[C, D](that: Map[A, C])(f: (B, Option[C]) => D)(implicit ev: Ordering[A]): TreeMap[A, D] =
    self.foldLeft(TreeMap.empty[A, D]) { case (m, (a, b)) =>
      m.updated(a, f(b, that.get(a)))
    }

  /**
   *  Merges two maps together according to the supplied function. The function
   * takes an `Ior` with the value from this map and/or the value from that map
   * for each key present in either map.
   *
   * @param that the map to merge with this one
   * @param f function that combines the values
   */
  def mergeAll[C, D](that: Map[A, C])(f: Ior[B, C] => D)(implicit ev: Ordering[A]): TreeMap[A, D] =
    (self.keySet  ++ that.keySet).foldLeft(TreeMap.empty[A, D]) { (m, a) =>
      Ior.fromOptions(self.get(a), that.get(a)).fold(m) { ior =>
        m.updated(a, f(ior))
      }
    }
}

trait ToTreeMapOps {
  implicit def ToTreeMapOps[A: Ordering, B](m: TreeMap[A, B]): TreeMapOps[A, B] =
    new TreeMapOps(m)
}

object treemap extends ToTreeMapCompanionOps with ToTreeMapOps