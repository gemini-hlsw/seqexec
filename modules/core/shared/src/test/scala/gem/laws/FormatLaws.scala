// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.laws

import gem.util.Format

final case class FormatLaws[A, B](fab: Format[A, B]) {

  def normalize(a: A): IsEq[Option[B]] =
    fab.normalize(a).flatMap(fab.parse) <-> fab.parse(a)
  
  def parseRoundTrip(a: A): IsEq[Option[A]] = {
    val oa = fab.normalize(a)
    oa.flatMap(fab.parse).map(fab.format) <-> oa
  }

  def formatRoundTrip(b: B): IsEq[Option[B]] =
    fab.parse(fab.format(b)) <-> Some(b)

}
