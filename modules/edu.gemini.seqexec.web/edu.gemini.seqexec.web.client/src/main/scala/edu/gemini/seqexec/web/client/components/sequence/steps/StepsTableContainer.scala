// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components.sequence.steps

import edu.gemini.seqexec.model.Model._
import edu.gemini.seqexec.web.client.components.SeqexecStyles
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.table.TableHeader
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._

import scalacss.ScalaCssReact._
import scalaz.std.AllInstances._
import scalaz.syntax.equal._

object StepConfigTable {
  final case class Props(step: Step)

  private val component = ScalaComponent.builder[Props]("StepConfigTable")
    .stateless
    .render_P ( p =>
      <.table(
        ^.cls := "ui selectable compact celled table unstackable",
        <.thead(
          <.tr(
            TableHeader(TableHeader.Props(collapsing = true), "Name"),
            TableHeader(TableHeader.Props(width = Width.Six), "Value")
          )
        ),
        <.tbody(
          p.step.config.flatMap {
            case (sub, c) =>
              c.map {
                case (k, v) =>
                  <.tr(
                    ^.classSet(
                      "positive" -> (sub === SystemName.instrument),
                      "warning"  -> (sub === SystemName.telescope)
                    ),
                    SeqexecStyles.observeConfig.when(k.startsWith("observe")),
                    SeqexecStyles.observeConfig.when(k.startsWith("ocs")),
                    <.td(k),
                    <.td(v)
                  )
              }
          }.toSeq.toTagMod
        )
      )
    ).build

  def apply(s: Step): Unmounted[Props, Unit, Unit] = component(Props(s))
}
