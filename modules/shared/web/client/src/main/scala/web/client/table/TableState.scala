// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package web.client.table

import scala.annotation.tailrec
import scala.math.max
import scala.math.min

import cats.Eq
import cats.data.NonEmptyList
import cats.syntax.all._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.raw.JsNumber
import monocle.Lens
import monocle.Optional
import monocle.function.Each._
import monocle.function.Index._
import react.common._
import react.virtualized._

/**
  * State of a table
  */
final case class TableState[A: Eq](userModified:   UserModified,
                                   scrollPosition: JsNumber,
                                   columns:        NonEmptyList[ColumnMeta[A]]) {

  val isModified: Boolean = userModified === IsModified

  // Reset visibility given the filters
  private def withVisibleCols(visibleFilter: A => Boolean): TableState[A] =
    (TableState.columns[A] ^|->> each).modify { c =>
      ColumnMeta.visible.set(visibleFilter(c.column))(c)
    }(this)

  // width set aside for fixed width columns
  private def fixedWidth(calculatedWidth: A => Option[Double]): Double =
    columns.collect {
      case ColumnMeta(c, _, _, true, FixedColumnWidth(x), _, _) =>
        calculatedWidth(c).getOrElse(x)
    }.sum

  // Changes the relative widths when a column is being dragged
  // delta is the change in percentage, negative to the left, positive to the right
  private def applyOffset(calculatedWidth: A => Option[Double],
                          column:          A,
                          Δ:               Double,
                          s:               Size): TableState[A] = {
    val cl      = columns.toList
    val indexOf = cl.indexWhere(_.column === column)
    val indexNx = cl.indexWhere((c: ColumnMeta[A]) => c.visible && c.isVariable, indexOf + 1)
    val ω       = s.width.toDouble - fixedWidth(calculatedWidth)
    val refCol  = cl.lift(indexOf)
    val refO    = (TableState.column[A](indexOf) ^|-> ColumnMeta.width)
    val nextCol = cl.lift(indexNx)
    val nextO   = (TableState.column[A](indexNx) ^|-> ColumnMeta.width)
    val id      = identity[TableState[A]] _

    if (Δ === 0.0) {
      this
    } else {
      // Build lenses to change the widths of the current and next cols
      val (rc, nc) = if (Δ < 0) {
        refCol match {
          case Some(ColumnMeta(_, _, _, _, VariableColumnWidth(p, m), _, _)) =>
            val pʹ  = p + Δ // percentage and delta
            val ωʹ  = ω * pʹ // new width
            val pʹʹ = max(m, ωʹ) / ω // new percentage with limit
            val Δʹ  = pʹʹ - p // new width with limit
            val rcL = refO.set(VariableColumnWidth(pʹʹ, m))
            val ncL = nextCol match {
              case Some(ColumnMeta(_, _, _, _, VariableColumnWidth(p, m), _, _)) =>
                nextO.set(VariableColumnWidth(p - Δʹ, m))
              case _ => id
            }
            (rcL, ncL)

          case _ => (id, id)
        }
      } else {
        nextCol match {
          case Some(ColumnMeta(_, _, _, _, VariableColumnWidth(p, m), _, _)) =>
            val pʹ  = p - Δ // percentage and delta
            val ωʹ  = ω * pʹ // new width
            val pʹʹ = max(m, ωʹ) / ω // new percentage with limit
            val Δʹ  = pʹʹ - p // new width with limit
            val ncL = nextO.set(VariableColumnWidth(pʹʹ, m))
            val rcL = refCol match {
              case Some(ColumnMeta(_, _, _, _, VariableColumnWidth(p, m), _, _)) =>
                refO.set(VariableColumnWidth(p - Δʹ, m))
              case _ => id
            }
            (rcL, ncL)

          case _ => (id, id)
        }
      }
      (TableState.userModified[A].set(IsModified) >>> rc >>> nc)(this)
    }
  }

  // Calculates the minimium variable width
  private def minVarWidth(calculatedWidth: A => Option[Double],
                          cols:            List[ColumnMeta[A]]): Double =
    cols.collect {
      case ColumnMeta(c, _, _, true, VariableColumnWidth(_, mw), _, _) =>
        calculatedWidth(c).map(max(mw, _)).getOrElse(mw)
    }.sum

  // based on the available width drop columns if needed
  @tailrec
  private def discardUntilUnder(calculatedWidth: A => Option[Double],
                                cols:            List[ColumnMeta[A]],
                                allowedWidth:    Double): List[ColumnMeta[A]] = {
    val minWidth           = minVarWidth(calculatedWidth, cols)
    val nonRemoveableCount = cols.count(c => c.removeable === 0 && c.visible)
    if (cols.length <= nonRemoveableCount || minWidth < allowedWidth) {
      cols
    } else {
      // Take the one with higher chance to be removed and hide it
      val hideOne = cols
        .filter(c => c.visible && c.removeable > 0)
        .sortBy(_.removeable)
        .lastOption
      hideOne match {
        case None => cols
        case Some(x) =>
          val updatedCols = cols.map {
            case c if x.column === c.column => ColumnMeta.visible.set(false)(c)
            case c                          => c
          }
          discardUntilUnder(calculatedWidth, updatedCols, allowedWidth)
      }
    }
  }

  private def distributePercentages(
    s:               Size,
    calculatedWidth: A => Option[Double]
  ): TableState[A] =
    if (s.width.toDouble === 0) {
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
      val minWidth           = minVarWidth(calculatedWidth, visibleCols)
      val totalVariableWidth = max(0.0, s.width.toDouble - fixedWidth(calculatedWidth))
      val cols =
        if (totalVariableWidth > requestedWidth) {
          // There is extra space on the table, lets distribute it among the cols
          val unallocatedWidth = totalVariableWidth - minWidth
          val weights = visibleCols.collect {
            case ColumnMeta(c, _, _, _, _, g, _) => (c, g)
          }.toMap
          // all columns weith
          val weightsSum = weights.values.sum
          // a segment is pixels per weight
          val segment = (unallocatedWidth / weightsSum)
          visibleCols.map {
            case m @ ColumnMeta(c, _, _, true, FixedColumnWidth(w), _, _) =>
              ColumnMeta
                .width[A]
                .set(FixedColumnWidth.unsafeFromDouble(
                  calculatedWidth(c).getOrElse(w)))(m)
            case m @ ColumnMeta(c, _, _, true, VariableColumnWidth(w, mw), g, _) =>
              val vc = VariableColumnWidth(
                calculatedWidth(c)
                  .map(w => (max(w, mw) + g * segment) / totalVariableWidth)
                  .getOrElse(w),
                mw)
              ColumnMeta.width[A].set(vc)(m)
            case x => x
          }
        } else {
          // There is less space on the table, we need to shrink
          // Lets drop columns if needed
          val reducedVisibleCols =
            if (totalVariableWidth < minWidth && s.width.toDouble > 0) {
              discardUntilUnder(calculatedWidth,
                                visibleCols,
                                totalVariableWidth)
            } else {
              visibleCols
            }
          // Update the columns with the correct percentage.
          reducedVisibleCols.map {
            case m @ ColumnMeta(c, _, _, true, FixedColumnWidth(w), _, _) =>
              ColumnMeta
                .width[A]
                .set(FixedColumnWidth.unsafeFromDouble(
                  calculatedWidth(c).getOrElse(w)))(m)
            case m @ ColumnMeta(c, _, _, true, VariableColumnWidth(w, mw), _, _) =>
              val vc = VariableColumnWidth(
                calculatedWidth(c)
                  .map(w => min(1.0, max(w, mw) / totalVariableWidth))
                  .getOrElse(w),
                mw)
              ColumnMeta.width[A].set(vc)(m)
            case x => x
          }
        }
      copy(columns = NonEmptyList.fromListUnsafe(cols))
    }

  // Table can call this to build the columns
  def columnBuilder(
    s:               Size,
    cb:              ColumnRenderArgs[A] => Table.ColumnArg,
    calculatedWidth: A => Option[Double] = TableState.NoInitialWidth
  ): List[Table.ColumnArg] = {
    val fw  = fixedWidth(calculatedWidth)
    val cl  = columns.toList.filter(_.visible)
    val vcl = cl.length
    // Last fixed column assuming the last one is indeed fixed
    val lf = cl.lastOption.filter(c => !c.isVariable) match {
      case Some(_) => max(0, cl.lastIndexWhere(c => !c.isVariable)) - 1
      case None    => vcl
    }
    cl.zipWithIndex
      .map {
        case (m @ ColumnMeta(_, _, _, _, FixedColumnWidth(w), _, _), i) =>
          cb(ColumnRenderArgs(m, i, w, false)).some

        case (m @ ColumnMeta(_, _, _, _, VariableColumnWidth(p, mw), _, _),
              i) =>
          val beforeLast = i < (lf - 1)
          val w          = max((s.width.toDouble - fw) * p, mw)
          cb(ColumnRenderArgs(m, i, w, beforeLast)).some

        case _ =>
          None
      }
      .mapFilter(identity)
  }

  // Reset all the columns to be visible
  private def resetVisibleColumns: TableState[A] =
    (TableState.columns[A] ^|->> each ^|-> ColumnMeta.visible).set(true)(this)

  // Table can call this to build the columns
  def recalculateWidths(
    s:               Size,
    visibleCols:     A => Boolean,
    calculatedWidth: A => Option[Double]
  ): TableState[A] =
    resetVisibleColumns
      .withVisibleCols(visibleCols)
      .distributePercentages(s, calculatedWidth)
      .normalizeColumnWidths(s)

  // normalize the percentage widths, As they may be more or less than 100% we
  // scale them to fit 100%
  private def normalizeColumnWidths(s: Size): TableState[A] =
    if (s.width.toDouble > 0) {
      // sum of all percentages to redistribute
      val percentagesSum = columns.collect {
        case ColumnMeta(_, _, _, true, VariableColumnWidth(p, _), _, _) => p
      }.sum
      (TableState.columns[A] ^|->> each).modify {
          case c @ ColumnMeta(_, _, _, true, VariableColumnWidth(p, min), _, _) =>
            c.copy(width = VariableColumnWidth(p / percentagesSum, min))
          case c =>
            c
        }(this)
    } else {
      this
    }

  def resizeColumn(
    column:          A,
    s:               Size,
    cb:              TableState[A] => Callback,
    visibleCols:     A => Boolean = TableState.AllColsVisible,
    calculatedWidth: A => Option[Double] = TableState.NoInitialWidth
  ): (String, JsNumber) => Callback =
    (_, dx) => {
      val deltaPct = dx.toDouble / (s.width.toDouble - fixedWidth(calculatedWidth))
      val st = withVisibleCols(visibleCols)
        .normalizeColumnWidths(s)
        .applyOffset(calculatedWidth, column, deltaPct, s)
      cb(st)
    }

}

object TableState {
  def AllColsVisible[A]: A => Boolean = (_: A) => true

  def NoInitialWidth[A]: A => Option[Double] = _ => None

  implicit def eqTs[A: Eq]: Eq[TableState[A]] =
    Eq.by(x => (x.userModified, x.scrollPosition.toDouble, x.columns))

  implicit val eqSize: Eq[Size] =
    Eq.by(x => (x.width.toDouble, x.height.toDouble))

  def userModified[A: Eq]: Lens[TableState[A], UserModified] =
    Lens[TableState[A], UserModified](_.userModified)(n =>
      a => a.copy(userModified = n))

  def columns[A: Eq]: Lens[TableState[A], NonEmptyList[ColumnMeta[A]]] =
    Lens[TableState[A], NonEmptyList[ColumnMeta[A]]](_.columns)(n =>
      a => a.copy(columns = n))

  def column[A: Eq](c: Int): Optional[TableState[A], ColumnMeta[A]] =
    columns[A] ^|-? index(c)

  def scrollPosition[A: Eq]: Lens[TableState[A], JsNumber] =
    Lens[TableState[A], JsNumber](_.scrollPosition)(n =>
      a => a.copy(scrollPosition = n))
}
