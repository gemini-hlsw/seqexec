// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.implicits._
import react.common.Css
import cats.implicits._
import seqexec.web.client.semanticui.elements.progress.Progress
import web.client.ReactProps

/**
  * Progress bar divided in steps
  */
final case class DividedProgress(
                                  sections            : List[DividedProgress.Label],
                                  sectionTotal        : DividedProgress.Quantity,
                                  value               : DividedProgress.Quantity,
                                  indicating          : Boolean = false,
                                  progress            : Boolean = false,
                                  completeSectionColor: Option[String] = None,
                                  ongoingSectionColor : Option[String] = None,
                                  progressCls         : List[Css] = Nil,
                                  barCls              : List[Css],
                                  labelCls            : List[Css] = Nil
                                ) extends ReactProps {
  @inline def render: VdomElement = DividedProgress.component(this)
}

object DividedProgress {
  type Props = DividedProgress

  type Label = String
  type Quantity = Int

  implicit val propsReuse: Reusability[Props] = Reusability.byRef

  protected val component = ScalaComponent
    .builder[Props]("DividedProgress")
    .stateless
    .render_P { p =>
      val sectionProgressStyles: List[Css] =
        SeqexecStyles.dividedProgressSectionLeft +:
          List.fill(p.sections.length - 2)(SeqexecStyles.dividedProgressSectionMiddle) :+
          SeqexecStyles.dividedProgressSectionRight

      val countSections = p.sections.length
      val completeSections = p.value / countSections
      val sectionValuesAndColors: List[(Quantity, Option[String])] =
        (List.fill(completeSections)((p.sectionTotal, p.completeSectionColor)) :+
          ((p.value % countSections, p.ongoingSectionColor))) ++
          List.fill(countSections - completeSections - 1)((0, None))

      val sectionBarStyles: List[Css] =
        if (completeSections === 0)
          List.empty
        else
          SeqexecStyles.dividedProgressBarLeft +:
            List.fill(completeSections - 1)(SeqexecStyles.dividedProgressBarMiddle) :+
            SeqexecStyles.dividedProgressBarRight

      <.span(
        SeqexecStyles.dividedProgress,
        p.sections.zip(sectionValuesAndColors).zip(sectionProgressStyles).zip(sectionBarStyles.padTo(countSections, Css.Zero)).toTagMod {
          case (((label, (sectionValue, sectionColor)), sectionProgressStyle), sectionBarStyle) =>
            Progress(Progress.Props(
              label,
              p.sectionTotal.toLong,
              sectionValue.toLong,
              p.indicating,
              p.progress,
              sectionColor,
              p.progressCls :+ sectionProgressStyle,
              p.barCls ++ List(sectionBarStyle, SeqexecStyles.dividedProgressBar),
              p.labelCls
            ))
        }
        )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}