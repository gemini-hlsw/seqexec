// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.Eq
import japgolly.scalajs.react.Reusability
import monocle.macros.Lenses

/**
 * Metadata for a column
 */
@Lenses
final case class ColumnMeta[A](
  column:     A,
  name:       String,
  label:      String,
  visible:    Boolean,
  width:      ColumnWidth,
  grow:       Int = 1,
  removeable: Int = 0
) {
  def isVariable: Boolean = width match {
    case _: FixedColumnWidth => false
    case _                   => true
  }
}

object ColumnMeta {
  implicit def eqCm[A: Eq]: Eq[ColumnMeta[A]] =
    Eq.by(x => (x.column, x.name, x.label, x.visible, x.width, x.grow, x.removeable))

  implicit def reuse[A: Reusability]: Reusability[ColumnMeta[A]] =
    Reusability.derive[ColumnMeta[A]]
}
