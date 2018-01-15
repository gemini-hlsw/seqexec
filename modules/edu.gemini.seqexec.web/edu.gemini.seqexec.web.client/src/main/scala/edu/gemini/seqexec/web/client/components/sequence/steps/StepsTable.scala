// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import scala.scalajs.js
import diode.react.ModelProxy
import edu.gemini.seqexec.model.Model.{Step}
// import edu.gemini.seqexec.web.client.ModelOps._
// import edu.gemini.seqexec.web.client.model.Pages.{SeqexecPages, SequenceConfigPage}
import edu.gemini.seqexec.web.client.model.Pages.SeqexecPages
// import edu.gemini.seqexec.web.client.actions.{FlipBreakpointStep, FlipSkipStep, NavigateSilentTo}
// import edu.gemini.seqexec.web.client.circuit.{ClientStatus, SeqexecCircuit, StepsTableFocus}
import edu.gemini.seqexec.web.client.circuit.{ClientStatus, StepsTableFocus}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
// import edu.gemini.seqexec.web.client.components.sequence.steps.OffsetFns._
// import edu.gemini.seqexec.web.client.lenses.stepTypeO
// import edu.gemini.seqexec.web.client.semanticui._
// import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon
// import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
// import edu.gemini.seqexec.web.client.semanticui.elements.label.Label
// import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
// import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
// import org.scalajs.dom.html.Div
//
import scalacss.ScalaCssReact._
import scalaz.std.AllInstances._
// import scalaz.syntax.equal._
// import scalaz.syntax.show._
// import scalaz.syntax.std.boolean._
import scalaz.syntax.std.option._
import react.virtualized._

/**
  * Container for a table with the steps
  */
object StepsTable {
  // ScalaJS defined trait
  // scalastyle:off
  trait StepRow extends js.Object {
    var idx: Int
  }
  // scalastyle:on
  object StepRow {
    @SuppressWarnings(Array("org.wartremover.warts.AsInstanceOf"))
    def apply(idx: Int): StepRow = {
      val p = (new js.Object).asInstanceOf[StepRow]
      p.idx = idx
      p
    }

    def unapply(l: StepRow): Option[(Int)] =
      Some((l.idx))

    val Zero: StepRow = apply(0)
  }

  final case class Props(router: RouterCtl[SeqexecPages], stepsTable: ModelProxy[(ClientStatus, Option[StepsTableFocus])], onStepToRun: Int => Callback) {
    def status: ClientStatus = stepsTable()._1
    def steps: Option[StepsTableFocus] = stepsTable()._2
    private val stepsList: List[Step] = ~steps.map(_.steps)
    def rowCount: Int = stepsList.length
    def rowGetter(idx: Int): StepRow = StepRow(idx)
    // // Find out if offsets should be displayed
    // val offsetsDisplay: OffsetsDisplay = stepsList.offsetsDisplay
  }

  private val IdxWidth = 20

  // Columns for the table
  private val columns = List(
    Column(Column.props(IdxWidth, "idx", label = "", disableSort = true)))

  def stepsTable(p: Props)(size: Size): VdomNode = {
    Table(
      Table.props(
        disableHeader = false,
        noRowsRenderer = () =>
          <.div(
            ^.cls := "ui center aligned segment noRows",
            ^.height := 270.px,
            "No log entries"
          ),
        overscanRowCount = SeqexecStyles.VirtTableStyles.overscanRowCount,
        height = size.height.toInt,
        rowCount = p.rowCount,
        rowHeight = SeqexecStyles.VirtTableStyles.rowHeight,
        // rowClassName = rowClassName(s) _,
        width = size.width.toInt,
        rowGetter = p.rowGetter _,
        // headerClassName = SeqexecStyles.logTableHeader.htmlClass,
        headerHeight = SeqexecStyles.VirtTableStyles.headerHeight),
      columns: _*).vdomElement
      // SeqexecStyles.stepsTable,
      // ^.onMouseLeave  --> mouseLeave,
      // StepsTableHeader(props.offsetsDisplay),
      // <.tbody(
      //   SeqexecStyles.stepsListBody,
      //   p.steps.zipWithIndex.flatMap {
      //     case (step, i) =>
      //       List(
      //         gutterCol(p.id, i, step, s, firstRunnableIndex(p.steps)),
      //         stepCols(props.router, props.status, p, i, p.state, step, props.offsetsDisplay)
      //       )
      //   }.toTagMod
      // )
  }

  private val component = ScalaComponent.builder[Props]("Steps")
    .render_P { p =>
      <.div(
        SeqexecStyles.stepsListPane,
        p.steps.whenDefined { tab =>
          tab.stepConfigDisplayed.map { i =>
            <.div("CONFIG")
          }.getOrElse {
            AutoSizer(AutoSizer.props(stepsTable(p), onResize = size => Callback.log(size)))
          }
        }
      )
    }.build

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
