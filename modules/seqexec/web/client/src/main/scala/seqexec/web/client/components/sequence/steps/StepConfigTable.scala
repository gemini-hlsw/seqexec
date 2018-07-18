// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import cats.implicits._
import react.virtualized._
import scala.scalajs.js
import seqexec.model.Model.{Step, SystemName}
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.actions.UpdateStepsTableState
import web.client.utils._
import web.client.table._

object StepConfigTable {
  type Backend = RenderScope[Props, TableState, Unit]

  final case class Props(step: Step, size: Size) {
    val settingsList: List[(SystemName, String, String)] =
      step.config.toList.flatMap {
        case (s, c) =>
          c.map {
            case (k, v) => (s, k, v)
          }
      }

    val rowCount: Int = settingsList.size

    def rowGetter(idx: Int): SettingsRow =
      settingsList.lift(idx).fold(SettingsRow.Zero)(Function.tupled(SettingsRow.apply))
  }

  // ScalaJS defined trait
  // scalastyle:off
  trait SettingsRow extends js.Object {
    var sub: SystemName
    var name: String
    var value: String
  }
  // scalastyle:on
  object SettingsRow {
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(sub: SystemName, name: String, value: String): SettingsRow = {
      val p = (new js.Object).asInstanceOf[SettingsRow]
      p.sub = sub
      p.name = name
      p.value = value
      p
    }

    def unapply(l: SettingsRow): Option[(SystemName, String, String)] =
      Some((l.sub, l.name, l.value))

    val Zero: SettingsRow = apply(SystemName.ocs, "", "")
  }

  sealed trait TableColumn
  case object NameColumn  extends TableColumn
  case object ValueColumn extends TableColumn

  object TableColumn {
    implicit val eq: Eq[TableColumn] = Eq.fromUniversalEquals
  }

  final case class ColumnMeta(column: TableColumn, name: String, label: String, width: Double)

  object ColumnMeta {
    val TableColumnMeta: ColumnMeta = ColumnMeta(NameColumn, "name", "Name", 0.5)
    val ValueColumnMeta: ColumnMeta = ColumnMeta(ValueColumn, "value", "Value", 0.5)
  }

  final case class TableState(userModified: Boolean,
                              columns: NonEmptyList[ColumnMeta]) {
    // Changes the relative widths when a column is being dragged
    def applyOffset(column: TableColumn, delta: Double): TableState = {
      val indexOf = columns.toList.indexWhere(_.column === column)
      // Shift the selected column and the next one
      val result = columns.toList.zipWithIndex.map {
        case (c @ ColumnMeta(_, _, _, x), idx) if idx === indexOf     =>
          c.copy(width = (x + delta))
        case (c @ ColumnMeta(_, _, _, x), idx) if idx === indexOf + 1 =>
          c.copy(width = (x - delta))
        case (c, _)                                                => c
      }
      copy(userModified = true, columns = NonEmptyList.fromListUnsafe(result))
    }
  }

  object TableState {
    val Zero: TableState = TableState(false, NonEmptyList.of(ColumnMeta.TableColumnMeta, ColumnMeta.ValueColumnMeta))
  }

  private def columns(b: Backend): List[Table.ColumnArg] = {
    val width = b.props.size.width
    // Tell the model to resize a column
    def resizeRow(c: TableColumn): (String, JsNumber) => Callback =
      (_, dx) =>
        b.modState { s =>
          val percentDelta = dx.toDouble / width
          s.applyOffset(c, percentDelta)
      }

    b.state.columns.zipWithIndex.map {
      case (ColumnMeta(c, name, label, percentage), i) if i < b.state.columns.length - 1 =>
        Column(Column.props(width * percentage, name, label = label, flexShrink = 0, flexGrow = 0, headerRenderer = resizableHeaderRenderer(resizeRow(c)), className = SeqexecStyles.paddedStepRow.htmlClass))
      case (ColumnMeta(_, name, label, percentage), _)                                   =>
        Column(Column.props(width * percentage, name, label = label, flexShrink = 0, flexGrow = 0, className = SeqexecStyles.paddedStepRow.htmlClass))
    }.toList
  }

  def rowClassName(p: Props)(i: Int): String = ((i, p.rowGetter(i)) match {
    case (-1, _)                                                  =>
      SeqexecStyles.headerRowStyle
    case (_, SettingsRow(s, _, _)) if s === SystemName.instrument =>
      SeqexecStyles.stepRow |+| SeqexecStyles.rowPositive
    case (_, SettingsRow(s, _, _)) if s === SystemName.telescope  =>
      SeqexecStyles.stepRow |+| SeqexecStyles.rowWarning
    case (_, SettingsRow(_, n, _)) if n.startsWith("observe:")    =>
      SeqexecStyles.stepRow |+| SeqexecStyles.observeConfig
    case (_, SettingsRow(_, n, _)) if n.startsWith("ocs:")        =>
      SeqexecStyles.stepRow |+| SeqexecStyles.observeConfig
    case _                                                        =>
      SeqexecStyles.stepRow
  }).htmlClass

  def settingsTableProps(p: Props): Table.Props = {
    Table.props(
      disableHeader = false,
      noRowsRenderer = () =>
        <.div(
          ^.cls := "ui center aligned segment noRows",
          ^.height := p.size.height.px,
          "No configuration for step"
        ),
      overscanRowCount = SeqexecStyles.overscanRowCount,
      height = p.size.height.toInt,
      rowCount = p.rowCount,
      rowHeight = SeqexecStyles.rowHeight,
      rowClassName = rowClassName(p) _,
      width = p.size.width.toInt,
      rowGetter = p.rowGetter _,
      scrollTop = 0,
      headerClassName = SeqexecStyles.tableHeader.htmlClass,
      headerHeight = SeqexecStyles.headerHeight)
  }

  private val component = ScalaComponent.builder[Props]("StepConfig")
    .stateless
    .render_P { p =>
      Table(settingsTableProps(p), columns(p): _*)
    }
    .build

  def apply(p: Props): Unmounted[Props, TableState, Unit] = component(p)
}
