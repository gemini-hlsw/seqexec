// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence.toolbars

import cats.syntax.all._
import japgolly.scalajs.react.ScalaComponent
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.form._
import react.semanticui.colors._
import react.semanticui.elements.label.Label
import react.semanticui.sizes._
import seqexec.model.SequenceState
import seqexec.model.UnknownTargetName
import seqexec.web.client.circuit.SequenceInfoFocus
import seqexec.web.client.components.SeqexecStyles
import seqexec.web.client.icons._

final case class SequenceInfo(p: SequenceInfoFocus) extends ReactProps[SequenceInfo](SequenceInfo.component)

/**
  * Display the name of the sequence and the observer
  */
object SequenceInfo {
  type Props = SequenceInfo

  val component =
    ScalaComponent
      .builder[Props]("SequenceInfo")
      .stateless
      .render_P { p =>
        val SequenceInfoFocus(isLogged, obsName, status, tName) = p.p
        val unknownTargetName: TagMod =
          Label(basic = true)(UnknownTargetName)
        val targetName = tName
          .filter(_.nonEmpty)
          .fold(unknownTargetName)(t => Label(basic = true)(t))
        Form(
          FormGroup(
            SeqexecStyles.fieldsNoBottom,
            FormField(
              Label(color = Green, size = Medium)(IconCheckmark, "Sequence Complete")
            ).when(status === SequenceState.Completed),
            FormField(
              Label(basic = true)(obsName)
            ).when(isLogged),
            FormField(
              targetName
            ).when(isLogged)
          )
        )
      }
      .build

}
