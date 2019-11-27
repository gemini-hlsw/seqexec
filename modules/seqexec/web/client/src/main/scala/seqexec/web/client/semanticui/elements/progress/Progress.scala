// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.semanticui.elements.progress

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.querki.jquery.$
import react.common._
import react.common.implicits._
import seqexec.web.client.semanticui._
import web.client.facades.semanticui.SemanticUIProgress._


/**
  * Produces a progress element using javascript
  */
final case class Progress(
  label:       String,
  total:       Int,
  value:       Int,
  indicating:  Boolean = false,
  progress:    Boolean = false,
  color:       Option[String] = None,
  progressCls: List[Css] = Nil,
  barCls:      List[Css],
  labelCls:    List[Css] = Nil
) extends ReactPropsWithChildren {
  @inline def render: Seq[CtorType.ChildArg] => VdomElement = Progress.component(this)
}

object Progress {
  type Props = Progress

  implicit val propsReuse: Reusability[Props] = Reusability.derive[Props]

  private val component = ScalaComponent
    .builder[Props]("Progress")
    .stateless
    .renderPC( (_, p, c) =>
      <.div(
        ^.cls := "ui progress",
        p.color.map(u => ^.cls := u).whenDefined,
        ^.classSet(
          "indicating" -> p.indicating
        ),
        p.progressCls,
        <.div(^.cls := "bar",
              p.barCls,
              <.div(^.cls := "progress").when(p.progress)),
        <.div(^.cls := "label",
              p.labelCls,
              p.label),
        c
      )
    )
    .componentDidUpdate(ctx =>
      Callback {
        ctx.getDOMNode.toElement.foreach { dom =>
          val percent =
            ctx.currentProps.value.toDouble / ctx.currentProps.total.toDouble
          $(dom).progress(
            JsProgressOptions
              .percent(100 * percent)
              .precision(0)
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
              .precision(0)
          )
        }
    })
    .configure(Reusability.shouldComponentUpdate)
    .build
}
