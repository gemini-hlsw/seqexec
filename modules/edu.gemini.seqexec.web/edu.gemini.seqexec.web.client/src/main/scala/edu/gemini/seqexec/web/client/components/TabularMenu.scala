package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.model.Model.{SequenceId, SequenceView, Instrument}
import edu.gemini.seqexec.web.client.model.{SeqexecCircuit, SelectToDisplay, SequencesOnDisplay}
import edu.gemini.seqexec.web.client.components.SeqexecUI.{InstrumentPage, RouterProps}
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.model.ModelOps._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}

import scalacss.ScalaCssReact._
import scalaz.syntax.equal._
import scalaz.std.string._
import scalaz.std.option._

/**
  * Menu with tabs
  */
object TabularMenu {
  case class TabItem(instrument: Instrument, id: Option[SequenceId], isActive: Boolean, dataItem: String, hasError: Boolean)
  case class Props(router: RouterProps, d: SequencesOnDisplay) {
    val tabs: List[TabItem] = d.instrumentSequences.map(a => TabItem(a.instrument, a.sequence().map(_.id), isActive = a.instrument === router.page.i, a.instrument, a.sequence().map(_.hasError).getOrElse(false))).toStream.toList
  }

  private val component = ScalaComponent.builder[Props]("TabularMenu")
    .stateless
    .render_P(p =>
      <.div(
        ^.cls := "ui attached tabular menu",
        p.tabs.map(t =>
          <.a(
            ^.cls := "item",
            ^.classSet(
              "active" -> t.isActive,
              "error"  -> t.hasError
            ),
            SeqexecStyles.errorTab.when(t.hasError),
            dataTab := t.dataItem,
            IconAttention.copyIcon(color = Some("red")).when(t.hasError),
            t.instrument
          )
        ).toTagMod
      )

    ).componentDidMount(ctx =>
      Callback {
        // Enable menu on Semantic UI
        import org.querki.jquery.$
        import edu.gemini.seqexec.web.client.semanticui.SemanticUI._

        $(ctx.getDOMNode).find(".item").tab(
          JsTabOptions
            .onVisible { (x: Instrument) =>
              val id = ctx.props.tabs.find(_.instrument === x).flatMap(_.id)
              val s: Option[SequenceView] = ctx.props.d.instrumentSequences.toStream.toList.find(_.sequence().map(_.id) === id).flatMap(_.sequence())
              val updateModelCB = s.map(seq => Callback(SeqexecCircuit.dispatch(SelectToDisplay(seq)))).getOrEmpty
              val updateRouteCB = ctx.props.router.router.set(InstrumentPage(x, id))
              // runNow as we are outside react loop
              (updateModelCB >> updateRouteCB).runNow()
            }
        )
      }
    ).build

  def apply(r: RouterProps, d: SequencesOnDisplay): Unmounted[Props, Unit, Unit] = component(Props(r, d))
}
