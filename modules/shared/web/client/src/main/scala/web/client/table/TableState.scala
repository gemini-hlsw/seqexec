// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import monocle.Lens
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.Callback
import react.virtualized._
import scala.math.max
import web.client.utils._
import react.common.syntax._

/**
  * State of a table
  */
final case class TableState[A: Eq](userModified:   UserModified,
                                   scrollPosition: JsNumber,
                                   columns:        NonEmptyList[ColumnMeta[A]]) {
  private def withVisibleCols(visibleFilter: (Size, A) => Boolean,
                              s:             Size): TableState[A] =
    TableState
      .columns[A]
      .modify(_.map {
        case c @ ColumnMeta(i, _, _, true, _) if !visibleFilter(s, i) =>
          ColumnMeta.visible.set(false)(c)
        case c => c
      })(this)

  // width set aside for fixed width columns
  private val fixedWidth = columns.collect {
    case ColumnMeta(_, _, _, true, FixedColumnWidth(x)) => x
  }.sum

  // Changes the relative widths when a column is being dragged
  // delta is the change in percentage, negative to the left, positive to the right
  def applyOffset(column: A, delta: Double, s: Size): TableState[A] = {
    val cl      = columns.toList
    val indexOf = cl.indexWhere(_.column === column)
    val width   = s.width - fixedWidth
    val refCol  = cl.lift(indexOf)
    val nextCol = cl.lift(indexOf + 1)

    if (delta === 0.0) {
      this
    } else {
      // Shift the selected column and the next one
      val result = columns.toList.zipWithIndex.map {
        case (c @ ColumnMeta(_, _, _, true, VariableColumnWidth(curPct, min)),
              idx) if idx === indexOf =>
          val newPct    = curPct + delta
          val newWidth  = max(min, width * newPct)
          val actualPct = newWidth / width

          val nextCanChange = nextCol.exists {
            case ColumnMeta(_, _, _, true, VariableColumnWidth(nextPct, nextMin)) =>
              nextMin < width * (nextPct - delta)
          }

          if (nextCanChange && newWidth <= s.width) {
            ColumnMeta.width.set(VariableColumnWidth(actualPct, min))(c)
          } else {
            c
          }
        case (c @ ColumnMeta(_, _, _, true, VariableColumnWidth(curPct, min)),
              idx) if idx === indexOf + 1 =>
          val newPct    = curPct - delta
          val newWidth  = max(min, width * newPct)
          val actualPct = newWidth / width

          val prevCanChange = refCol.exists {
            case ColumnMeta(_, _, _, true, VariableColumnWidth(prevPct, prevMin)) =>
              prevMin < width * (prevPct + delta)
          }

          if (prevCanChange && newWidth <= width) {
            ColumnMeta.width.set(VariableColumnWidth(actualPct, min))(c)
          } else {
            c
          }
        case (c, _) => c
      }
      copy(userModified = IsModified,
           columns      = NonEmptyList.fromListUnsafe(result))
    }
  }

  // Return the width of a column from the actual column width
  def widthOf(column: A, s: Size): Double =
    columns
      .filter(c => c.column === column && c.visible)
      .map(_.width)
      .map {
        case FixedColumnWidth(w)       => w.toDouble
        case VariableColumnWidth(f, m) => max(m, f * (s.width - fixedWidth))
      }
      .foldMap(identity)

  // Table can call this to build the columns
  def columnBuilder(
    s:           Size,
    visibleCols: (Size, A) => Boolean,
    cb:          ColumnRenderArgs[A] => Table.ColumnArg
  ): List[Table.ColumnArg] = {
    val vc  = withVisibleCols(visibleCols, s)
    val vcl = vc.columns.count(_.visible)
    vc.normalizeColumnWidths(s)
      .columns
      .toList
      .zipWithIndex
      .map {
        case (m @ ColumnMeta(_, _, _, true, FixedColumnWidth(w)), i) =>
          cb(ColumnRenderArgs(m, i, w, false)).some

        case (m @ ColumnMeta(_, _, _, true, VariableColumnWidth(p, mw)), i) =>
          val beforeLast = i < (vcl - 1)
          val w          = max((s.width - fixedWidth) * p, mw)
          cb(ColumnRenderArgs(m, i, w, beforeLast)).some

        case _ =>
          None
      }
      .collect { case Some(c) => c }
  }

  // normalize the width
  private def normalizeColumnWidths(s: Size): TableState[A] =
    if (s.width > 0) {
      // sum of all percentages to redistribute
      val percentagesSum = columns.collect {
        case ColumnMeta(_, _, _, true, VariableColumnWidth(p, m)) =>
          scala.math.max(p, m / (s.width - fixedWidth))
      }.sum
      (TableState
        .columns[A]
        .modify(_.map {
          case c @ ColumnMeta(_, _, _, true, VariableColumnWidth(p, min)) =>
            c.copy(width = VariableColumnWidth(p / percentagesSum, min))
          case c =>
            c
        }))(this)
    } else {
      this
    }

  def resizeRow(
    column: A,
    s:      Size,
    cb:     TableState[A] => Callback
  ): (String, JsNumber) => Callback =
    (_, dx) => {
      val delta = dx.toDouble / (s.width - fixedWidth)
      val st = normalizeColumnWidths(s).applyOffset(column, delta, s)
      cb(st)
    }

}

object TableState {
  def AllColsVisible[A](s: Size, a: A): Boolean = true

  implicit def eqTs[A: Eq]: Eq[TableState[A]] =
    Eq.by(x => (x.userModified, x.scrollPosition.toDouble, x.columns))

  def userModified[A: Eq]: Lens[TableState[A], UserModified] =
    Lens[TableState[A], UserModified](_.userModified)(n =>
      a => a.copy(userModified = n))

  def columns[A: Eq]: Lens[TableState[A], NonEmptyList[ColumnMeta[A]]] =
    Lens[TableState[A], NonEmptyList[ColumnMeta[A]]](_.columns)(n =>
      a => a.copy(columns = n))

  def scrollPosition[A: Eq]: Lens[TableState[A], JsNumber] =
    Lens[TableState[A], JsNumber](_.scrollPosition)(n =>
      a => a.copy(scrollPosition = n))
}
