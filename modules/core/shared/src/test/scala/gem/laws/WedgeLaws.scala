// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.laws

import cats.Eq
import cats.implicits._
import gem.optics.Wedge

final case class WedgeLaws[A, B](fab: Wedge[A, B]) {

  def normalizeA(a: A): IsEq[B] =
    fab.get(fab.normalizeA(a)) <-> fab.get(a)

  def normalizeB(b: B): IsEq[A] =
    fab.reverseGet(fab.normalizeB(b)) <-> fab.reverseGet(b)

  def normalizedReverseGetRoundTrip(b: B): IsEq[B] = {
    val bʹ = fab.normalizeB(b)
    (fab.reverseGet andThen fab.get)(bʹ) <-> bʹ
  }

  def normalizedGetRoundTrip(a: A): IsEq[A] = {
    val aʹ = fab.normalizeA(a)
    (fab.reverseGet compose fab.get)(aʹ) <-> aʹ
  }

  // Demonstrate coverage
  def demonstratesCoverageA(b: A)(implicit ev: Eq[A]): Boolean =
    (fab.reverseGet compose fab.get)(b) =!= b

  def demonstratesCoverageB(b: B)(implicit ev: Eq[B]): Boolean =
    (fab.reverseGet andThen fab.get)(b) =!= b

}
