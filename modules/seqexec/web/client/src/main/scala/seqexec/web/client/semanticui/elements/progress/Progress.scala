// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.progress

import japgolly.scalajs.react.component.Scala.Unmounted
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.querki.jquery.$
import web.client.facades.semanticui.SemanticUIProgress._
import web.client.style._

/**
  * Produces a progress element using javascript
  */
object Progress {
  @SuppressWarnings(Array("org.wartremover.warts.DefaultArguments"))
  final case class Props(label:       String,
                         total:       Long,
                         value:       Long,
                         indicating:  Boolean = false,
                         progress:    Boolean = false,
                         progressCls: List[GStyle] = Nil,
                         barCls:      List[GStyle],
                         labelCls:    List[GStyle] = Nil)

  private val component = ScalaComponent
    .builder[Props]("Progress")
    .stateless
    .renderPC { (_, p, c) =>
      <.div(
        ^.cls := "ui progress",
        ^.classSet(
          "indicating" -> p.indicating
        ),
        p.progressCls.map(geminiStyleToTagMod).toTagMod,
        <.div(^.cls := "bar",
              p.barCls.map(geminiStyleToTagMod).toTagMod,
              <.div(^.cls := "progress").when(p.progress)),
        <.div(^.cls := "label",
              p.labelCls.map(geminiStyleToTagMod).toTagMod,
              p.label),
        c
      )
    }
    .componentDidUpdate(ctx =>
      Callback {
        ctx.getDOMNode.toElement.foreach { dom =>
          val percent =
            ctx.currentProps.value.toDouble / ctx.currentProps.total.toDouble
          $(dom).progress(
            JsProgressOptions
              .percent(100 * percent)
          )
        }
    })
    .componentDidMount(ctx =>
      Callback {
        ctx.getDOMNode.toElement.foreach { dom =>
          val percent =
            ctx.props.value.toDouble / ctx.props.total.toDouble
          $(dom).progress(
            JsProgressOptions
              .percent(100 * percent)
          )
        }
    })
    .build

  def apply(p: Props, children: VdomElement*): Unmounted[Props, Unit, Unit] =
    component(p)(children: _*)
}
