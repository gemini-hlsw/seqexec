package edu.gemini.seqexec.web.client.components.sequence

import diode.ModelR
import diode.react.ModelProxy
import edu.gemini.seqexec.web.client.model.ModelOps._
import edu.gemini.seqexec.web.client.components.TextMenuSegment
import edu.gemini.seqexec.web.client.model._
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.message.IconMessage
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon.IconInbox
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{CallbackTo, ScalaComponent, ScalazReact}
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.ScalazReact._

import scalaz.syntax.apply.{^ => _, _}
import scalaz.std.option._

object SequenceStepsTableContainer {
  case class Props(p: ModelProxy[InstrumentTabAndStatus])
  case class State(nextStepToRun: Int)

  private val ST = ReactS.Fix[State]

  def updateStepToRun(step: Int): ScalazReact.ReactST[CallbackTo, State, Unit] =
    ST.set(State(step)).liftCB

    private val component = ScalaComponent.builder[Props]("SequenceStepsTableContainer")
    .initialState(State(0))
    .renderPS { ($, p, s) =>
      val status = p.p().status
      val it = p.p().tab.map(_.tab)
      val sequence = it.flatMap(_.sequence())
      (it |@| sequence) { (t, s) =>
        <.div(
          ^.cls := "ui raised secondary segment",
          t.stepConfigDisplayed.fold{
            (if (status.isLogged)
            SequenceDefaultToolbar(SequenceDefaultToolbar.Props(s, status, s.nextStepToRun))
            else
            SequenceAnonymousToolbar(SequenceAnonymousToolbar.Props(s))): VdomElement
          }
          (step => StepConfigToolbar(StepConfigToolbar.Props(s, status.isLogged, step))): VdomElement,
          StepsTableContainer(StepsTableContainer.Props(s, status, t.stepConfigDisplayed, s.nextStepToRun, x => $.runState(updateStepToRun(x))))
        ): VdomElement
      }.getOrElse(<.div(): VdomElement)
    }.componentWillMount { f =>
      val nextStep = for {
        t <- f.props.p().tab
        s <- t.tab.sequence()
        n <- s.nextStepToRun
      } yield n
      f.modState(_.copy(nextStepToRun = nextStep.getOrElse(0)))
    }.build

    def apply(p: ModelProxy[InstrumentTabAndStatus]): Unmounted[Props, State, Unit] = component(Props(p))
  }

  /**
  * Content of a single tab with a sequence
  */
  object SequenceTabContent {

    case class Props(p: ModelProxy[InstrumentTabAndStatus])

    private val component = ScalaComponent.builder[Props]("SequenceTabContent")
    .stateless
    .render_P { p =>
      val sequenceTab = p.p()
      sequenceTab match {
        case i @ InstrumentTabAndStatus(status, Some(InstrumentSequence(tab, active))) =>
        <.div(
          ^.cls := "ui bottom attached tab segment",
          ^.classSet(
            "active" -> active
          ),
          dataTab := tab.instrument,
          tab.sequence().fold(IconMessage(IconMessage.Props(IconInbox, Some("No sequence loaded"), IconMessage.Style.Warning)): VdomNode) { s =>
            SequenceStepsTableContainer(p.p): VdomNode
          }
        )
        case _ =>
        <.div()
      }
    }
    .build

    def apply(p: ModelProxy[InstrumentTabAndStatus]) = component(Props(p))
  }

  /**
  * Contains the area with tabs and the sequence body
  */
  object SequenceTabsBody {
    case class Props(s: ClientStatus, d: SequencesOnDisplay)

    // TODO Consider GN/GS
    val instrumentConnects = InstrumentNames.instruments.list.toList.map(i => SeqexecCircuit.connect(SeqexecCircuit.instrumentTabAndStatus(i)))

    private val component = ScalaComponent.builder[Unit]("SequenceTabsBody")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "twelve wide computer twelve wide tablet sixteen wide mobile column",
        InstrumentsTabs(),
        instrumentConnects.map(c => c(SequenceTabContent.apply)).toTagMod
      )
    ).build

    def apply(): Unmounted[Unit, Unit, Unit] =
      component()
    }

    object SequenceHeadersAndTable {
      private val headerSideBarConnect = SeqexecCircuit.connect(SeqexecCircuit.headerSideBarReader)

      private val component = ScalaComponent.builder[Unit]("SequenceHeadersAndTable")
      .stateless
      .render_P(p =>
        <.div(
          ^.cls := "row",
          <.div(
            ^.cls := "four wide column computer tablet only",
            headerSideBarConnect(HeadersSideBar.apply)
          ),
          SequenceTabsBody()
        )
      ) .build

      def apply(): Unmounted[Unit, Unit, Unit] = component()
    }

    /**
    * Contains all the tabs for the sequences available in parallel
    * All connects at this level, be careful about adding connects below here
    */
    object SequenceTabs {
      private val component = ScalaComponent.builder[Unit]("SequenceTabs")
      .stateless
      .render_P( p =>
        <.div(
          ^.cls := "ui bottom attached segment",
          <.div(
            ^.cls := "ui two column vertically divided grid",
            SequenceHeadersAndTable()
          )
        )
      )
      .build

      def apply(): Unmounted[Unit, Unit, Unit] = component()
    }

    object SequenceArea {
      type SequencesModel = ModelR[SeqexecAppRootModel, (ClientStatus, SequencesOnDisplay)]
      type HeadersSideBarModel = ModelR[SeqexecAppRootModel, HeaderSideBarReader]

      private val component = ScalaComponent.builder[Unit]("QueueTableSection")
      .stateless
      .render_P( p =>
        <.div(
          ^.cls := "ui raised segments container",
          TextMenuSegment("Running Sequences", "key.sequences.menu"),
          SequenceTabs()
        )
      ).build

      def apply(): Unmounted[Unit, Unit, Unit] = component()
    }
