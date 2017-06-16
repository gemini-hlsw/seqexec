package edu.gemini.seqexec.web.client.components

import edu.gemini.seqexec.model.Model.Instrument
import edu.gemini.seqexec.web.client.model.SequencesOnDisplay
import edu.gemini.seqexec.web.client.components.SeqexecUI.{InstrumentPage, RouterProps}
import edu.gemini.seqexec.web.client.semanticui._
import edu.gemini.seqexec.web.client.semanticui.elements.icon.Icon._
import edu.gemini.seqexec.web.client.model.ModelOps._
import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{Callback, ScalaComponent}

import scalacss.ScalaCssReact._
import scalaz.Zipper
import scalaz.syntax.equal._
import scalaz.std.string._

/**
  * Menu with tabs
  */
object TabularMenu {
  case class TabItem(title: String, isActive: Boolean, dataItem: String, hasError: Boolean)
  case class Props(router: RouterProps, tabs: List[TabItem])

  def sequencesTabs(router: RouterProps, d: SequencesOnDisplay): Zipper[TabItem] = d.instrumentSequences.map(a => TabItem(a.instrument, isActive = a.instrument === router.page.i, a.instrument, a.sequence().map(_.hasError).getOrElse(false)))

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
            t.title
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
            // runNow as we are outside react loop
            .onVisible((x: Instrument) => ctx.props.router.router.set(InstrumentPage(x)).runNow())
        )
      }
    ).build

  def apply(r: RouterProps, p: SequencesOnDisplay): Unmounted[Props, Unit, Unit] = component(Props(r, sequencesTabs(r, p).toStream.toList))
}
