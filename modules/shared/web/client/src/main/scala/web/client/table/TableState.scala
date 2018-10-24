// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import monocle.Lens
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.Callback
import react.virtualized._
import web.client.utils._

/**
  * State of a table
  */
final case class TableState[A: Eq](userModified:   UserModified,
                                   scrollPosition: JsNumber,
                                   columns:        NonEmptyList[ColumnMeta[A]]) {

  // Changes the relative widths when a column is being dragged
  def applyOffset(column: A, delta: Double, s: Size): TableState[A] = {
    val indexOf = columns.toList.indexWhere(_.column === column)
    // Shift the selected column and the next one
    val result = columns.toList.zipWithIndex.map {
      case (c @ ColumnMeta(_, _, _, true, PercentageColumnWidth(x, min)), idx) if idx === indexOf     =>
        val nv = x + delta
        if (nv >= 0 && nv <= 1) {
          c.copy(width = PercentageColumnWidth(nv, min))
        } else {
          c
        }
      case (c @ ColumnMeta(_, _, _, true, PercentageColumnWidth(x, min)), idx) if idx === indexOf + 1 =>
        val nv = scala.math.max(min / s.width, x - delta)
        if (nv >= 0 && nv <= 1) {
          c.copy(width = PercentageColumnWidth(nv, min))
        } else {
          c
        }
      case (c, _)                                                                                => c
    }
    copy(userModified = IsModified,
         columns      = NonEmptyList.fromListUnsafe(result))
  }

  // Return the width of a column from the actual column width
  def widthOf(column: A, s: Size): Double =
    columns
      .filter(c => c.column === column && c.visible)
      .map(_.width)
      .map {
        case FixedColumnWidth(w)         => w.toDouble
        case PercentageColumnWidth(f, m) => scala.math.max(m, f * s.width)
      }
      .foldMap(identity)

  // Table can call this to build the columns
  def columnBuilder(
    s:  Size,
    cb: ColumnRenderArgs[A] => Table.ColumnArg): List[Table.ColumnArg] = {
    val disposableWidth = math.max(0, s.width.toDouble - columns.collect {
      case ColumnMeta(_, _, _, true, FixedColumnWidth(x)) => x
    }.sum)
    normalizeColumnsPercentages(s).columns.toList.zipWithIndex.map {
      case (m @ ColumnMeta(_, _, _, true, FixedColumnWidth(w)), i) =>
        cb.apply(ColumnRenderArgs(m, i, w, false))
      case (m @ ColumnMeta(_, _, _, true, PercentageColumnWidth(p, mw)), i) =>
        cb.apply(
          ColumnRenderArgs(m,
                           i,
                           scala.math.max(mw, p * disposableWidth),
                           i < (columns.length - 1)))
    }
  }

  // normalize the percentages
  def normalizeColumnsPercentages(s: Size): TableState[A] = {
    val percentagesSum = columns.collect {
      case ColumnMeta(_, _, _, true, PercentageColumnWidth(x, m)) => scala.math.max(x, m / s.width)
    }.sum
    TableState
      .columns[A]
      .modify(_.map {
        case c @ ColumnMeta(_, _, _, true, PercentageColumnWidth(x, min)) =>
          c.copy(width = PercentageColumnWidth(x / percentagesSum, min))
        case c                                                            =>
          c
      })(this)
  }

  // Tell the model to resize a column
  def resizeRow(
    column: A,
    s:      Size,
    cb:     TableState[A] => Callback): (String, JsNumber) => Callback =
    (_, dx) => {
      val percentDelta = dx.toDouble / s.width.toDouble
      val st           = applyOffset(column, percentDelta, s)
      cb(st)
    }
}

object TableState {
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
