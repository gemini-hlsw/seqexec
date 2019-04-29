// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import monocle.Lens
import monocle.function.Each._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.Callback
import react.common.syntax._
import react.virtualized._
import scala.math.max
import scala.math.min
import scala.annotation.tailrec

/**
  * State of a table
  */
final case class TableState[A: Eq](userModified:   UserModified,
                                   scrollPosition: JsNumber,
                                   columns:        NonEmptyList[ColumnMeta[A]]) {

  val isModified: Boolean = userModified === IsModified

  // Reset visibility given the filters
  private def withVisibleCols(visibleFilter: A => Boolean): TableState[A] =
    (TableState
      .columns[A] ^|->> each).modify { c =>
        ColumnMeta.visible.set(visibleFilter(c.column))(c)
      }(this)

  // width set aside for fixed width columns
  private def fixedWidth(calculatedWidth: A => Option[Double]): Double = columns.collect {
    case ColumnMeta(c, _, _, true, FixedColumnWidth(x), _, _) =>
      calculatedWidth(c).getOrElse(x)
  }.sum

  // Changes the relative widths when a column is being dragged
  // delta is the change in percentage, negative to the left, positive to the right
  def applyOffset(calculatedWidth: A => Option[Double], column: A, delta: Double, s: Size): TableState[A] = {
    val cl      = columns.toList.filter(_.visible)
    val indexOf = cl.indexWhere(_.column === column)
    val fw      = fixedWidth(calculatedWidth)
    val width   = s.width - fw
    val refCol  = cl.lift(indexOf)
    val nextCol = cl.lift(indexOf + 1)
    println("Apply off ")
              println(column)
              println(indexOf)

    if (delta === 0.0) {
      this
    } else {
      // Shift the selected column and the next one
      val result = cl.zipWithIndex.map {
        case (c @ ColumnMeta(_, _, _, true, VariableColumnWidth(curPct, min), _, _),
              idx) if idx === indexOf =>
              println(curPct)
          val newPct    = curPct + delta
          val newWidth  = max(min, width * newPct)
          val actualPct = newWidth / width

          val nextCanChange = nextCol.exists {
            case ColumnMeta(_, _, _, true, VariableColumnWidth(nextPct, nextMin), _, _) =>
              nextMin < width * (nextPct - delta)
          }

          if (nextCanChange && newWidth <= s.width) {
            ColumnMeta.width.set(VariableColumnWidth(actualPct, min))(c)
          } else {
            c
          }

        case (c @ ColumnMeta(_, _, _, true, VariableColumnWidth(curPct, min), _, _),
              idx) if idx === indexOf + 1 =>
          val newPct    = curPct - delta
          val newWidth  = max(min, width * newPct)
          val actualPct = newWidth / width

          val prevCanChange = refCol.exists {
            case ColumnMeta(_, _, _, true, VariableColumnWidth(prevPct, prevMin), _, _) =>
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

  private def minVarWidth(calculatedWidth: A => Option[Double], cols: List[ColumnMeta[A]]): Double =
    cols.collect {
      case ColumnMeta(c, _, _, true, VariableColumnWidth(_, mw), _, _) =>
        calculatedWidth(c).map(max(mw, _)).getOrElse(mw)
    }.sum

  @tailrec
  private def discardUntilUnder(calculatedWidth: A => Option[Double], cols: List[ColumnMeta[A]], allowedWidth: Double): List[ColumnMeta[A]] = {
    val minWidth = minVarWidth(calculatedWidth, cols)
    val nonRemoveableCount = cols.count(c => c.removeable === 0 && c.visible)
    if (cols.length <= nonRemoveableCount || minWidth < allowedWidth) {
      cols
    } else {
      // Take the one with higher chance to be removed and hide it
      val hideOne = cols.filter(_.removeable > 0).filter(_.visible).sortBy(_.removeable).lastOption
      hideOne match {
        case None => cols
        case Some(x) =>
          val updatedCols = cols.map {
            case c if x.column === c.column => ColumnMeta.visible.set(false)(c)
            case c => c
          }
          discardUntilUnder(calculatedWidth, updatedCols, allowedWidth)
      }
    }
  }

  // scalastyle:off
  @SuppressWarnings(
    Array("org.wartremover.warts.NonUnitStatements",
          "org.wartremover.warts.Throw"))
  private def distributePercentages(
    s:               Size,
    calculatedWidth: A => Option[Double]): TableState[A] =
    if (isModified || s.width === 0) {
      // If we have been modified don't redistribute
      this
    } else {
      // list of visible columns
      val visibleCols = columns.toList
      // Width according to the suggested sizes
      val requestedWidth = visibleCols.collect {
        case ColumnMeta(c, _, _, true, FixedColumnWidth(w), _, _) =>
          calculatedWidth(c).getOrElse(w)
        case ColumnMeta(c, _, _, true, VariableColumnWidth(_, mw), _, _) =>
          calculatedWidth(c).map(max(mw, _)).getOrElse(mw)
      }.sum
      println("requestedWidth")
      println(requestedWidth)
      val minWidth = minVarWidth(calculatedWidth, visibleCols)
      val totalVariableWidth = s.width - fixedWidth(calculatedWidth)
      // val cs = visibleCols.toList.map(_.column).mkString(",")
      // println(cs)
      val cols  =
        if (totalVariableWidth > requestedWidth) {
          // There is extra space on the table, lets distribute it among the cols
          val unallocatedWidth = totalVariableWidth - minWidth
          val weights = visibleCols.collect {
            case ColumnMeta(c, _, _, _, _, g, _) =>
              (c, g)
          }.toMap
          // all columns weith
          val weightsSum = weights.values.sum
          // a segment is pixels per weight
          val segment = (unallocatedWidth / weightsSum)
          visibleCols.map {
            case m @ ColumnMeta(c, _, _, true, FixedColumnWidth(w), _, _) =>
              ColumnMeta.width[A].set(FixedColumnWidth.unsafeFromDouble(calculatedWidth(c).getOrElse(w)))(m)
            case m @ ColumnMeta(c, _, _, true, VariableColumnWidth(w, mw), g, _) =>
              val vc = VariableColumnWidth(
                calculatedWidth(c)
                  .map(w => (max(w, mw) + g * segment) / totalVariableWidth)
                  .getOrElse(w), mw)
              ColumnMeta.width[A].set(vc)(m)
            case x => x
          }
        } else {
          // There is less space on the table, we need to shrink
          // println("Drop")
          // Lets drop columns if needed
          val reducedVisibleCols = if (totalVariableWidth < minWidth && s.width > 0) {
            discardUntilUnder(calculatedWidth, visibleCols, totalVariableWidth)
          } else {
            visibleCols
          }
          // println(reducedVisibleCols.length)
          // println(reducedVisibleCols.map(x => s"${x.column} -> ${x.width}").mkString(","))
          // Update the columns with the correct percentage.
          reducedVisibleCols.map {
            case m @ ColumnMeta(c, _, _, true, FixedColumnWidth(w), _, _) =>
              ColumnMeta.width[A].set(
                FixedColumnWidth.unsafeFromDouble(
                  calculatedWidth(c).getOrElse(w)))(m)
            case m @ ColumnMeta(c, _, _, true, VariableColumnWidth(w, mw), _, _) =>
              // println(c)
              // println(calculatedWidth(c).map(w => min(1.0, max(w, mw) / totalVariableWidth)))
              val vc = VariableColumnWidth(calculatedWidth(c)
                  .map(w => min(1.0, max(w, mw) / totalVariableWidth))
                                             .getOrElse(w), mw)
                                             println("done")
              ColumnMeta.width[A].set(vc)(m)
            case x => x
          }
        }
      copy(columns = NonEmptyList.fromListUnsafe(cols))
    }
  // scalastyle:on

  // Table can call this to build the columns
  def columnBuilder(
    s:               Size,
    visibleCols:     A => Boolean,
    calculatedWidth: A => Option[Double],
    cb:              ColumnRenderArgs[A] => Table.ColumnArg
  ): List[Table.ColumnArg] = {
    val vc =
      withVisibleCols(visibleCols)
        .distributePercentages(s, calculatedWidth)
    val vcl = vc.columns.count(_.visible)

    val ts = vc.normalizeColumnWidths(s)
    // recalculate as the widths way have varied
    val fixedWidth = ts.fixedWidth(calculatedWidth)
    println(s"Builder")
    ts.columns.toList.zipWithIndex
      .map {
        case (m @ ColumnMeta(_, _, _, true, FixedColumnWidth(w), _, _), i) =>
          cb(ColumnRenderArgs(m, i, w, false)).some

        case (m @ ColumnMeta(_, _, _, true, VariableColumnWidth(p, mw), _, _),
              i) =>
          val beforeLast = i < (vcl - 1)
          val w          = max((s.width - fixedWidth) * p, mw)
          cb(ColumnRenderArgs(m, i, w, beforeLast)).some

        case _ =>
          None
      }
      .mapFilter(identity)
  }

  // Table can call this to build the columns
  def columnBuilder2(
    s:               Size,
    calculatedWidth: A => Option[Double],
    cb:              ColumnRenderArgs[A] => Table.ColumnArg
  ): List[Table.ColumnArg] = {
    val fw = fixedWidth(calculatedWidth)
    val vcl = columns.count(_.visible)
    columns.toList.zipWithIndex
      .map {
        case (m @ ColumnMeta(_, _, _, true, FixedColumnWidth(w), _, _), i) =>
          cb(ColumnRenderArgs(m, i, w, false)).some

        case (m @ ColumnMeta(_, _, _, true, VariableColumnWidth(p, mw), _, _),
              i) =>
          val beforeLast = i < (vcl - 1)
          val w          = max((s.width - fw) * p, mw)
          cb(ColumnRenderArgs(m, i, w, beforeLast)).some

        case _ =>
          None
      }
      .mapFilter(identity)
  }

  def printCols: String =
    columns.toList.map {
      case ColumnMeta(c, _, _, _, FixedColumnWidth(w), _, _) =>
        s"$c -> Fix: $w"
      case ColumnMeta(c, _, _, _, VariableColumnWidth(w, mw), _, _) =>
        s"$c -> Var: $w $mw"
    }.mkString(",")

  def printVisibleCols: String =
    columns.toList.filter(_.visible).map {
      case ColumnMeta(c, _, _, _, FixedColumnWidth(w), _, _) =>
        s"$c -> Fix: $w"
      case ColumnMeta(c, _, _, _, VariableColumnWidth(w, mw), _, _) =>
        s"$c -> Var: $w $mw"
    }.mkString(",")

  // Reset all the columns to be visible
  private def resetVisibleColumns: TableState[A] =
    (TableState.columns[A] ^|->> each ^|-> ColumnMeta.visible).set(true)(this)

  // Table can call this to build the columns
  def recalculateWidths(
    s:               Size,
    visibleCols:     A => Boolean,
    calculatedWidth: A => Option[Double]
  ): TableState[A] = {
    println("recal")
    resetVisibleColumns
      .withVisibleCols(visibleCols)
      .distributePercentages(s, calculatedWidth)
      .normalizeColumnWidths(s)
    }

  // Table can call this to build the columns
  def columnBuilderC(
    s:  Size,
    cb: ColumnRenderArgs[A] => Table.ColumnArg
  ): List[Table.ColumnArg] =
    columnBuilder(s, TableState.AllColsVisible, TableState.NoInitialWidth, cb)

  // normalize the percentage widths, As they may be more or less than 100% we
  // scale them to fit 100%
  private def normalizeColumnWidths(s: Size): TableState[A] =
    if (s.width > 0) {
      // sum of all percentages to redistribute
      val percentagesSum = columns.collect {
        case ColumnMeta(_, _, _, true, VariableColumnWidth(p, _), _, _) =>
          p
      }.sum
      (TableState
        .columns[A] ^|->> each)
        .modify{
          case c @ ColumnMeta(_, _, _, true, VariableColumnWidth(p, min), _, _) =>
            c.copy(width = VariableColumnWidth(p / percentagesSum, min))
          case c =>
            c
        }(this)
    } else {
      this
    }

  def resizeRow(
    column:      A,
    s:           Size,
    visibleCols: A => Boolean,
    calculatedWidth: A => Option[Double],
    cb:          TableState[A] => Callback
  ): (String, JsNumber) => Callback =
    (_, dx) => {
      val delta = dx.toDouble / (s.width - fixedWidth(calculatedWidth))
      // println(pr.mkString(","))
      val st = withVisibleCols(visibleCols)
        .normalizeColumnWidths(s)
        .applyOffset(calculatedWidth, column, delta, s)
      println("after")
      // println(pr3.mkString(","))
      cb(st)
    }

  def resizeRowB(
    column: A,
    s:      Size,
    cb:     TableState[A] => Callback
  ): (String, JsNumber) => Callback =
    resizeRow(column, s, TableState.AllColsVisible, _ => none, cb)
}

object TableState {
  def AllColsVisible[A](a: A): Boolean = true

  def NoInitialWidth[A]: A => Option[Double] = _ => None

  implicit def eqTs[A: Eq]: Eq[TableState[A]] =
    Eq.by(x => (x.userModified, x.scrollPosition.toDouble, x.columns))

  implicit val eqSize: Eq[Size] =
    Eq.by(x => (x.width, x.height))

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
