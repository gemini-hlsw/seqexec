// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import scala.scalajs.js
// import diode.react.ModelProxy
// import edu.gemini.seqexec.model.Model.{Instrument, StandardStep, Step, StepState, StepType}
import edu.gemini.seqexec.model.Model.{Step, SystemName}
// import edu.gemini.seqexec.web.client.lenses._
// import edu.gemini.seqexec.web.client.model.Pages.SeqexecPages
// import edu.gemini.seqexec.web.client.circuit.{ClientStatus, StepsTableFocus}
import edu.gemini.seqexec.web.client.components.SeqexecStyles
// import edu.gemini.seqexec.web.client.components.sequence.steps.OffsetFns._
// import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
// import edu.gemini.seqexec.web.client.semanticui.{Size => SSize}
import japgolly.scalajs.react._
// import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
//
// import scalacss.ScalaCssReact._
import scalacss._
import scalaz.std.AllInstances._
import scalaz.syntax.traverse._
import scalaz.syntax.equal._
// import scalaz._
// import Scalaz._
import react.virtualized._

object StepConfigTable {
  private val CssSettings = scalacss.devOrProdDefaults
  import CssSettings._

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
      settingsList.index(idx).fold(SettingsRow.Zero){ case (s, n, v) => SettingsRow(s, n, v)}
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

  private def columns(p: Props): List[Table.ColumnArg] =
    List(
      Column(Column.props(p.size.width.toInt / 2, "name", label = "Name")),
      Column(Column.props(p.size.width.toInt / 2, "value", label = "Value"))
    )

  def rowClassName(p: Props)(i: Int): String = ((i, p.rowGetter(i)) match {
    case (-1, _)                                                   =>
      SeqexecStyles.headerRowStyle
    case (_, SettingsRow(s, _, _)) if s === SystemName.instrument =>
      SeqexecStyles.stepRow + SeqexecStyles.rowPositive
    case (_, SettingsRow(s, _, _)) if s === SystemName.telescope  =>
      SeqexecStyles.stepRow + SeqexecStyles.rowWarning
    case (_, SettingsRow(_, n, _)) if n.startsWith("observe:")    =>
      SeqexecStyles.stepRow + SeqexecStyles.observeConfig
    case (_, SettingsRow(_, n, _)) if n.startsWith("ocs:")        =>
      SeqexecStyles.stepRow + SeqexecStyles.observeConfig
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

  def apply(p: Props): Unmounted[Props, Unit, Unit] = component(p)
}
