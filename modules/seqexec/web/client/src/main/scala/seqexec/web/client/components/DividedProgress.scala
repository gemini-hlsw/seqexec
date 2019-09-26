package seqexec.web.client.components

import japgolly.scalajs.react.{Reusability, ScalaComponent}
import japgolly.scalajs.react.vdom.html_<^.VdomElement
import react.common.Css
import seqexec.web.client.semanticui.elements.progress.Progress
import web.client.ReactProps

/**
  * Progress bar divided in steps
  *
  * NOT IMPLEMENTED YET. JUST RENDERS A REGULAR PROGRESS BAR.
  *
  */
final case class DividedProgress(
  label:       String,
  total:       Long,
  value:       Long,
  indicating:  Boolean = false,
  progress:    Boolean = false,
  color:       Option[String] = None,
  progressCls: List[Css] = Nil,
  barCls:      List[Css],
  labelCls:    List[Css] = Nil
) extends ReactProps {
  @inline def render: VdomElement = DividedProgress.component(this)
}

object DividedProgress {
  type Props = DividedProgress

  implicit val propsReuse: Reusability[Props] = Reusability.byRef

  protected val component = ScalaComponent
    .builder[Props]("DividedProgress")
    .stateless
    .render_P(p =>
                Progress(Progress.Props(
                  p.label,
                  p.total,
                  p.value,
                  p.indicating,
                  p.progress,
                  p.color,
                  p.progressCls,
                  p.barCls,
                  p.labelCls
                ))
              )
    .configure(Reusability.shouldComponentUpdate)
    .build
}
