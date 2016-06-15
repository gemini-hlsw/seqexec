package edu.gemini.seqexec.web.client.components.sequence

import diode.react.ReactPot._
import edu.gemini.seqexec.model.UserDetails
import edu.gemini.seqexec.web.client.components.{SeqexecStyles, TabularMenu, TextMenuSegment}
import edu.gemini.seqexec.web.client.components.TabularMenu.TabItem
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.button.Button
import edu.gemini.seqexec.web.client.semanticui.elements.divider.Divider
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconCaretRight, IconInbox, IconPause, IconPlay}
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.{IconAttention, IconCheckmark, IconCircleNotched, IconStop}
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconChevronLeft
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.common.{Sequence, SequenceState, StepState}
import edu.gemini.seqexec.web.client.services.HtmlConstants.iconEmpty
import japgolly.scalajs.react.vdom.prefix_<^._
import japgolly.scalajs.react.{Callback, ReactComponentB, ReactElement}

import scalacss.ScalaCssReact._
import scalaz.syntax.show._

/**
  * Container for a table with the steps
  */
object SequenceStepsTableContainer {
  case class Props(s: Sequence, user: Option[UserDetails], stepConfigDisplayed: Option[Int])

  def requestRun(s: Sequence): Callback = Callback {SeqexecCircuit.dispatch(RequestRun(s))}

  def requestPause(s: Sequence): Callback = Callback.log("Request pause")

  def requestStop(s: Sequence): Callback = Callback {SeqexecCircuit.dispatch(RequestStop(s))}

  def displayStepDetails(s: Sequence, i: Int): Callback = Callback {SeqexecCircuit.dispatch(ShowStep(s, i))}

  def backToSequence(s: Sequence): Callback = Callback {SeqexecCircuit.dispatch(UnShowStep(s))}

  val component = ReactComponentB[Props]("HeadersSideBar")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui raised secondary segment",
        p.stepConfigDisplayed.fold {
          <.div(
            ^.cls := "row",
            p.user.isDefined && p.s.state == SequenceState.Abort ?=
              <.h3(
                ^.cls := "ui red header",
                "Sequence aborted"
              ),
            p.user.isDefined && p.s.state == SequenceState.Completed ?=
              <.h3(
                ^.cls := "ui green header",
                "Sequence completed"
              ),
            p.user.isDefined && p.s.state == SequenceState.NotRunning ?=
              Button(Button.Props(icon = Some(IconPlay), labeled = true, onClick = requestRun(p.s)), "Run"),
            p.user.isDefined && p.s.state == SequenceState.Running ?=
              Button(Button.Props(icon = Some(IconPause), labeled = true, disabled = true, onClick = requestPause(p.s)), "Pause"),
            p.user.isDefined && p.s.state == SequenceState.Running ?=
              Button(Button.Props(icon = Some(IconStop), labeled = true, onClick = requestStop(p.s)), "Stop")
          )
        } { i =>
          <.div(
            ^.cls := "row",
            Button(Button.Props(icon = Some(IconChevronLeft), onClick = backToSequence(p.s)), "Back"),
            <.h5(
              ^.cls :="ui header",
              SeqexecStyles.inline,
              s" Configuration for step ${i + 1}"
            )
          )
        },
        Divider(),
        <.div(
          ^.cls := "ui row scroll pane",
          SeqexecStyles.stepsListPane,
          p.stepConfigDisplayed.map { i =>
            val step = p.s.steps.steps.find(_.id == i)
            <.table(
              ^.cls := "ui selectable compact celled table unstackable",
              <.thead(
                <.tr(
                  <.th(
                    ^.cls := "collapsing",
                    "Name"
                  ),
                  <.th(
                    ^.cls := "six wide",
                    "Value"
                  )
                )
              ),
              <.tbody(
                step.map(_.config).getOrElse(Nil).map( c =>
                  <.tr(
                    ^.classSet(
                      "positive" -> c.key.startsWith("instrument"),
                      "warning"  -> c.key.startsWith("telescope")
                    ),
                    c.key.startsWith("observe") ?= SeqexecStyles.observeConfig,
                    c.key.startsWith("ocs") ?= SeqexecStyles.observeConfig,
                    <.td(
                      c.key
                    ),
                    <.td(
                      c.value
                    )
                  )
                )
              )
            )
          }.getOrElse {
            <.table(
              ^.cls := "ui selectable compact celled table unstackable",
              <.thead(
                <.tr(
                  <.th(
                    ^.cls := "collapsing",
                    iconEmpty
                  ),
                  <.th(
                    ^.cls := "collapsing",
                    "Step"
                  ),
                  <.th(
                    ^.cls := "six wide",
                    "State"
                  ),
                  <.th(
                    ^.cls := "ten wide",
                    "File"
                  ),
                  <.th(
                    ^.cls := "collapsing",
                    "Config"
                  )
                )
              ),
              <.tbody(
                p.s.steps.steps.map(s =>
                  <.tr(
                    ^.classSet(
                      "positive" -> (s.state == StepState.Done),
                      "warning"  -> (s.state == StepState.Running),
                      "negative" -> (s.state == StepState.Error),
                      "negative" -> (s.state == StepState.Abort)
                    ),
                    <.td(
                      s.state match {
                        case StepState.Done    => IconCheckmark
                        case StepState.Running => IconCircleNotched.copy(IconCircleNotched.p.copy(loading = true))
                        case StepState.Error   => IconAttention
                        case _                 => iconEmpty
                      }
                    ),
                    <.td(s.id + 1),
                    <.td(s.state.shows),
                    <.td(s.file.getOrElse(""): String),
                    <.td(
                      ^.cls := "collapsing right aligned",
                      IconCaretRight.copyIcon(onClick = displayStepDetails(p.s, s.id))
                    )
                  )
                )
              )
            )
          }
        )
      )
    )
    .build

  def apply(s: Sequence, user: Option[UserDetails], stepConfigDisplayed: Option[Int]) = component(Props(s, user,  stepConfigDisplayed))
}

/**
  * Content of a single tab with a sequence
  */
object SequenceTabContent {
  def seqConnect(s: Sequence) = SeqexecCircuit.connect(SeqexecCircuit.sequenceReader(s.id))

  case class Props(isActive: Boolean, user: Option[UserDetails], st: SequenceTab)

  val component = ReactComponentB[Props]("SequenceTabContent")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui bottom attached tab segment",
        ^.classSet(
          "active" -> p.isActive
        ),
        dataTab := p.st.instrument,
        p.st.sequence().render { s =>
          seqConnect(s)(u => u().map(t => SequenceStepsTableContainer(t, p.user, p.st.stepConfigDisplayed)).getOrElse(<.div(): ReactElement))
        },
        p.st.sequence().renderEmpty(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)))
      )
    )
    .build

  def apply(p: Props) = component(p)
}

/**
  * Contains all the tabs for the sequences available in parallel
  */
object SequenceTabs {
  val logConnect = SeqexecCircuit.connect(_.globalLog)

  case class Props(user: Option[UserDetails], sequences: SequencesOnDisplay)

  def sequencesTabs(d: SequencesOnDisplay) = d.instrumentSequences.map(a => TabItem(a.instrument, isActive = a == d.instrumentSequences.focus, a.instrument))
  def tabContents(user: Option[UserDetails], d: SequencesOnDisplay) = d.instrumentSequences.map(a => SequenceTabContent.Props(isActive = a == d.instrumentSequences.focus, user, a)).toStream

  val component = ReactComponentB[Props]("SequenceTabs")
    .stateless
    .render_P( p =>
      <.div(
        ^.cls := "ui bottom attached segment",
        <.div(
          ^.cls := "ui two column vertically divided grid",
          <.div(
            ^.cls := "row",
            SeqexecStyles.rowNoPadding,
            <.div(
              ^.cls := "four wide column computer tablet only",
              HeadersSideBar()
            ),
            <.div(
              ^.cls := "twelve wide computer twelve wide tablet sixteen wide mobile column",
              TabularMenu(sequencesTabs(p.sequences).toStream.toList),
              tabContents(p.user, p.sequences).map(SequenceTabContent(_))
            )
          ),
          <.div(
            ^.cls := "row computer only",
            <.div(
              ^.cls := "sixteen wide column",
              logConnect(LogArea(_))
            )
          )
        )
      )
    )
    .build

  def apply(user: Option[UserDetails], sequences: SequencesOnDisplay) = component(Props(user, sequences))
}

object SequenceArea {

  val connect = SeqexecCircuit.connect(m => (m.user, m.sequencesOnDisplay))

  val component = ReactComponentB[Unit]("QueueTableSection")
    .stateless
    .render( _ =>
      <.div(
        ^.cls := "ui raised segments container",
        TextMenuSegment("Running Sequences"),
        connect(p => SequenceTabs(p()._1, p()._2))
      )
    ).build

  def apply() = component()
}
