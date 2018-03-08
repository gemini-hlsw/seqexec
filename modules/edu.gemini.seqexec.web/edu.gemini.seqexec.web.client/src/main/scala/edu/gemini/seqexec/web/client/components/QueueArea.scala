// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components

import scala.scalajs.js
import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{Instrument, SequenceId, DaytimeCalibrationTargetName, SequenceState}
import edu.gemini.seqexec.web.client.circuit._
import edu.gemini.seqexec.web.client.actions._
import edu.gemini.seqexec.web.client.model.Pages._
import edu.gemini.seqexec.web.client.ModelOps._
import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconAttention, IconCheckmark, IconCircleNotched, IconSelectedRadio}
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.extra.router.RouterCtl
import react.virtualized._

import scalacss.ScalaCssReact._
import scalaz.syntax.show._
import scalaz.syntax.traverse._
import scalaz.syntax.std.boolean._
import scalaz.std.AllInstances._

object QueueTableBody {
  private val CssSettings = scalacss.devOrProdDefaults
  import CssSettings._

  final case class Props(ctl: RouterCtl[SeqexecPages], sequences: ModelProxy[StatusAndLoadedSequencesFocus]) {
    private lazy val sequencesList = sequences().sequences
    def rowGetter(i: Int): QueueRow = sequencesList.index(i).map { s =>
        QueueRow(s.id, s.status, s.instrument, s.targetName, s.name, s.active, s.runningStep)
      }.getOrElse(QueueRow.Zero)

    def rowCount: Int =
      sequencesList.size
  }

  // ScalaJS defined trait
  // scalastyle:off
  trait QueueRow extends js.Object {
    var obsId: SequenceId
    var status: SequenceState
    var instrument: Instrument
    var targetName: Option[String]
    var name: String
    var active: Boolean
    var runningStep: Option[RunningStep]
  }
  // scalastyle:on
  object QueueRow {
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(obsId: SequenceId, status: SequenceState, instrument: Instrument, targetName: Option[String], name: String, active: Boolean, runningStep: Option[RunningStep]): QueueRow = {
      val p = (new js.Object).asInstanceOf[QueueRow]
      p.obsId = obsId
      p.status = status
      p.instrument = instrument
      p.targetName = targetName
      p.name = name
      p.active = active
      p.runningStep = runningStep
      p
    }

    def unapply(l: QueueRow): Option[(SequenceId, SequenceState, Instrument, Option[String], String, Boolean, Option[RunningStep])] =
      Some((l.obsId, l.status, l.instrument, l.targetName, l.name, l.active, l.runningStep))

    val Zero: QueueRow = apply("", SequenceState.Idle, Instrument.F2, None, "", false, None)
  }

  def showSequence(p: Props, i: Instrument, id: SequenceId): Callback =
    // Request to display the selected sequence
    p.sequences.dispatchCB(NavigateTo(SequencePage(i, id, 0)))

  private def linkTo(p: Props, page: SequencePage)(mod: TagMod*) =
    <.a(
      ^.href := p.ctl.urlFor(page).value,
      ^.onClick --> showSequence(p, page.instrument, page.obsId),
      p.ctl.setOnLinkClick(page),
      mod.toTagMod
    )

  private def linkedTextRenderer(p: Props)(f: QueueRow => TagMod): CellRenderer[js.Object, js.Object, QueueRow] = (_, _, _, row: QueueRow, _) => {
    val page = SequencePage(row.instrument, row.obsId, 0)
    linkTo(p, page)(SeqexecStyles.queueTextColumn, f(row))
  }

  private def obsIdRenderer(p: Props) = linkedTextRenderer(p){ r => <.p(SeqexecStyles.queueText, r.obsId) }

  private def obsNameRenderer(p: Props) = linkedTextRenderer(p){ r => <.p(SeqexecStyles.queueText, r.name) }

  private def stateRenderer(p: Props) = linkedTextRenderer(p){ r =>
    val stepAtText = r.status.shows + r.runningStep.map(u => s" ${u.shows}").getOrElse("")
    <.p(SeqexecStyles.queueText, stepAtText)
  }

  private def instrumentRenderer(p: Props) = linkedTextRenderer(p){ r => <.p(SeqexecStyles.queueText, r.instrument.shows) }

  val daytimeCalibrationTargetName: TagMod =
    <.span(
      SeqexecStyles.daytimeCal,
      DaytimeCalibrationTargetName)

  private def targetRenderer(p: Props) = linkedTextRenderer(p){ r =>
    val targetName = r.targetName.fold(daytimeCalibrationTargetName)(x => x: TagMod)
    <.p(SeqexecStyles.queueText, targetName)
  }

  def statusIconRenderer(p: Props): CellRenderer[js.Object, js.Object, QueueRow] = (_, _, _, row: QueueRow, _) => {
    val icon: TagMod =
      row.status match {
        case SequenceState.Completed     => IconCheckmark.copyIcon(fitted = true, extraStyles = List(SeqexecStyles.selectedIcon))
        case SequenceState.Running(_, _) => IconCircleNotched.copyIcon(fitted = true, loading = true, extraStyles = List(SeqexecStyles.runningIcon))
        case SequenceState.Failed(_)     => IconAttention.copyIcon(fitted = true, extraStyles = List(SeqexecStyles.selectedIcon))
        case _                           => if (row.active) IconSelectedRadio.copyIcon(fitted = true, extraStyles = List(SeqexecStyles.selectedIcon)) else iconEmpty
      }

      val page = SequencePage(row.instrument, row.obsId, 0)
      linkTo(p, page) (
        SeqexecStyles.queueIconColumn,
        icon
      )
  }

  val statusHeaderRenderer: HeaderRenderer[js.Object] = (_, _, _, _, _, _) =>
    <.div(
      ^.title := "Control",
      ^.width := 12.px
    )

  private def columns(p: Props): List[Table.ColumnArg] = {
    val isLogged = p.sequences().isLogged
    val regularColumns = List(
      Column(Column.props(12, "obsId", flexShrink = 0, flexGrow = 0, label = "", cellRenderer = statusIconRenderer(p), headerRenderer = statusHeaderRenderer, className = SeqexecStyles.queueIconColumn.htmlClass)),
      Column(Column.props(140, "obsId", flexShrink = 0, flexGrow = 0, label = "Obs. ID", cellRenderer = obsIdRenderer(p), className = SeqexecStyles.queueTextColumn.htmlClass)),
      Column(Column.props(80, "state", flexShrink = 0, flexGrow = 0, label = "State", cellRenderer = stateRenderer(p), className = SeqexecStyles.queueTextColumn.htmlClass)),
      Column(Column.props(100, "instrument", flexShrink = 0, flexGrow = 0, label = "Instrument", cellRenderer = instrumentRenderer(p), className = SeqexecStyles.queueTextColumn.htmlClass))
    )
    val loggedInColumns = List(
      Column(Column.props(140, "target", flexShrink = 1, flexGrow = 1, label = "Target", cellRenderer = targetRenderer(p), className = SeqexecStyles.queueTextColumn.htmlClass)),
      Column(Column.props(140, "obsName", flexShrink = 1, flexGrow = 1, label = "Obs. Name", cellRenderer = obsNameRenderer(p), className = SeqexecStyles.queueTextColumn.htmlClass))
    )
    isLogged.fold(regularColumns ::: loggedInColumns, regularColumns)
  }

  def rowClassName(p: Props)(i: Int): String = ((i, p.rowGetter(i)) match {
    case (-1, _)                                                             =>
      SeqexecStyles.headerRowStyle
    case (_, QueueRow(_, s, _, _, _, _, _)) if s == SequenceState.Completed  =>
      SeqexecStyles.stepRow + SeqexecStyles.rowPositive
    case (_, QueueRow(_, s, _, _, _, _, _)) if s.isRunning                   =>
      SeqexecStyles.stepRow + SeqexecStyles.rowWarning
    case (_, QueueRow(_, s, _, _, _, _, _)) if s.isError                     =>
      SeqexecStyles.stepRow + SeqexecStyles.rowNegative
    case (_, QueueRow(_, s, _, _, _, active, _)) if active && !s.isInProcess =>
      SeqexecStyles.stepRow + SeqexecStyles.rowActive
    case _                                                                   =>
      SeqexecStyles.stepRow
  }).htmlClass

  def table(p: Props)(size: Size): VdomNode =
    Table(
      Table.props(
        disableHeader = false,
        noRowsRenderer = () =>
          <.div(
            ^.cls := "ui center aligned segment noRows",
            SeqexecStyles.noRowsSegment,
            ^.height := 213.px,
            "Queue empty"
          ),
        overscanRowCount = SeqexecStyles.overscanRowCount,
        height = 213,
        rowCount = p.rowCount,
        rowHeight = SeqexecStyles.rowHeight,
        rowClassName = rowClassName(p) _,
        width = size.width.toInt,
        rowGetter = p.rowGetter _,
        headerClassName = SeqexecStyles.tableHeader.htmlClass,
        headerHeight = SeqexecStyles.headerHeight),
      columns(p): _*).vdomElement

  private val component = ScalaComponent.builder[Props]("QueueTableBody")
    .render_P ( p =>
      AutoSizer(AutoSizer.props(table(p), disableHeight = true))
    )
    .build

  def apply(ctl: RouterCtl[SeqexecPages], p: ModelProxy[StatusAndLoadedSequencesFocus]): Unmounted[Props, Unit, Unit] = component(Props(ctl, p))

}

/**
  * Container for the queue table
  */
object QueueTableSection {
  private val sequencesConnect = SeqexecCircuit.connect(SeqexecCircuit.statusAndLoadedSequencesReader)

  private val component = ScalaComponent.builder[RouterCtl[SeqexecPages]]("QueueTableSection")
    .stateless
    .render_P(p =>
      <.div(
        SeqexecStyles.queueListPane,
        sequencesConnect(c => QueueTableBody(p, c))
      )
    ).build

  def apply(ctl: RouterCtl[SeqexecPages]): Unmounted[RouterCtl[SeqexecPages], Unit, Unit] = component(ctl)

}

/**
  * Displays the elements on the queue
  */
object QueueArea {

  private val component = ScalaComponent.builder[RouterCtl[SeqexecPages]]("QueueArea")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "twelve wide column",
        QueueTableSection(p)
      )
    )
    .build

  def apply(ctl: RouterCtl[SeqexecPages]): Unmounted[RouterCtl[SeqexecPages], Unit, Unit] = component(ctl)

}
