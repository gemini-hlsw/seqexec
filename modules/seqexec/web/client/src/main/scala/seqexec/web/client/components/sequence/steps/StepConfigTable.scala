// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.steps

import cats.Eq
import cats.data.NonEmptyList
import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.component.builder.Lifecycle.RenderScope
import japgolly.scalajs.react.Reusability
import react.virtualized._
import react.common._
import react.common.implicits._
import scala.scalajs.js
import scala.math.max
import seqexec.model.Step
import seqexec.model.enum.SystemName
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.components.TableContainer
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.actions.UpdateStepsConfigTableState
import seqexec.web.client.reusability._
import web.client.table._

final case class StepConfigTable(
  step: Step,
  startState: TableState[StepConfigTable.TableColumn]
) extends ReactProps {
  @inline def render: VdomElement = StepConfigTable.component(this)

  val settingsList: List[(SystemName, String, String)] =
    step.config.toList.flatMap {
      case (s, c) =>
        c.map {
          case (k, v) => (s, k, v)
        }
    }

  val rowCount: Int = settingsList.size

  def rowGetter(idx: Int): StepConfigTable.SettingsRow =
    settingsList
      .lift(idx)
      .fold(StepConfigTable.SettingsRow.zero)(Function.tupled(StepConfigTable.SettingsRow.apply))
}

object StepConfigTable {
  sealed trait TableColumn extends Product with Serializable
  case object NameColumn  extends TableColumn
  case object ValueColumn extends TableColumn

  object TableColumn {
    implicit val eq: Eq[TableColumn]             = Eq.fromUniversalEquals
    implicit val reuse: Reusability[TableColumn] = Reusability.byRef
  }

  type Props = StepConfigTable

  type Backend = RenderScope[Props, TableState[TableColumn], Unit]

  implicit val propsReuse: Reusability[Props] =
    Reusability.by(p => (p.settingsList, p.startState))

  // ScalaJS defined trait
  trait SettingsRow extends js.Object {
    var sub  : SystemName
    var name : String
    var value: String
  }

  object SettingsRow {

    def apply(sub: SystemName, name: String, value: String): SettingsRow = {
      val p = (new js.Object).asInstanceOf[SettingsRow]
      p.sub   = sub
      p.name  = name
      p.value = value
      p
    }

    def unapply(l: SettingsRow): Option[(SystemName, String, String)] =
      Some((l.sub, l.name, l.value))

    def zero: SettingsRow = apply(SystemName.Ocs, "", "")
  }

  val TableColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    column  = NameColumn,
    name    = "name",
    label   = "Name",
    visible = true,
    width = VariableColumnWidth.unsafeFromDouble(
      percentage = 0.5,
      minWidth   = 57.3833 + SeqexecStyles.TableBorderWidth)
  )

  val ValueColumnMeta: ColumnMeta[TableColumn] = ColumnMeta[TableColumn](
    column  = ValueColumn,
    name    = "value",
    label   = "Value",
    visible = true,
    width =
      VariableColumnWidth.unsafeFromDouble(percentage = 0.5, minWidth = 60.0))

  val InitialTableState: TableState[TableColumn] = TableState[TableColumn](
    userModified   = NotModified,
    scrollPosition = 0,
    columns        = NonEmptyList.of(TableColumnMeta, ValueColumnMeta))

  private def colBuilder(
    b:    Backend,
    size: Size
  ): ColumnRenderArgs[TableColumn] => Table.ColumnArg = tb => {
    def updateState(s: TableState[TableColumn]): Callback =
      b.setState(s) >> SeqexecCircuit.dispatchCB(UpdateStepsConfigTableState(s))

    tb match {
      case ColumnRenderArgs(meta, _, width, true) =>
        Column(
          Column.propsNoFlex(
            width          = width,
            dataKey        = meta.name,
            label          = meta.label,
            headerRenderer = resizableHeaderRenderer(b.state.resizeColumn(meta.column, size, updateState)),
            className      = SeqexecStyles.paddedStepRow.htmlClass
          ))
      case ColumnRenderArgs(meta, _, width, false) =>
        Column(
          Column.propsNoFlex(
            width     = width,
            dataKey   = meta.name,
            label     = meta.label,
            className = SeqexecStyles.paddedStepRow.htmlClass))
    }
  }

  def updateScrollPosition(b: Backend, pos: JsNumber): Callback = {
    val s = TableState.scrollPosition[TableColumn].set(pos)(b.state)
    b.setState(s) *> SeqexecCircuit.dispatchCB(UpdateStepsConfigTableState(s))
  }

  def rowClassName(p: Props)(i: Int): String =
    ((i, p.rowGetter(i)) match {
      case (-1, _) =>
        SeqexecStyles.headerRowStyle
      case (_, SettingsRow(s, _, _)) if s === SystemName.Instrument =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowPositive
      case (_, SettingsRow(s, _, _)) if s === SystemName.Telescope  =>
        SeqexecStyles.stepRow |+| SeqexecStyles.rowWarning
      case (_, SettingsRow(_, n, _)) if n.startsWith("observe:")    =>
        SeqexecStyles.stepRow |+| SeqexecStyles.observeConfig
      case (_, SettingsRow(_, n, _)) if n.startsWith("ocs:")        =>
        SeqexecStyles.stepRow |+| SeqexecStyles.observeConfig
      case _                                                        =>
        SeqexecStyles.stepRow
    }).htmlClass

  def settingsTableProps(b: Backend, size: Size): Table.Props =
    Table.props(
      disableHeader = false,
      noRowsRenderer = () =>
        <.div(
          ^.cls := "ui center aligned segment noRows",
          ^.height := size.height.px,
          "No configuration for step"
      ),
      overscanRowCount = SeqexecStyles.overscanRowCount,
      height           = max(1, size.height.toInt),
      rowCount         = b.props.rowCount,
      rowHeight        = SeqexecStyles.rowHeight,
      rowClassName     = rowClassName(b.props) _,
      width            = max(1, size.width.toInt),
      rowGetter        = b.props.rowGetter _,
      scrollTop        = b.state.scrollPosition,
      headerClassName  = SeqexecStyles.tableHeader.htmlClass,
      onScroll         = (_, _, pos) => updateScrollPosition(b, pos),
      headerHeight     = SeqexecStyles.headerHeight
    )

  protected val component = ScalaComponent
    .builder[Props]("StepConfig")
    .initialStateFromProps(_.startState)
    .render ( b =>
      TableContainer(TableContainer.Props(true, size =>
        if (size.width > 0) {
          Table(settingsTableProps(b, size),
                b.state.columnBuilder(size, colBuilder(b, size)): _*)
        } else {
          <.div()
        },
      onResize = _ => Callback.empty))
    )
    .configure(Reusability.shouldComponentUpdate)
    .build
}
