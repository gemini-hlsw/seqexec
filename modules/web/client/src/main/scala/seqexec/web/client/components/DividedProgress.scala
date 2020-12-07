// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import scala.scalajs.js.JSConverters._

import cats.syntax.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.semanticui.SemanticColor
import react.semanticui.modules.progress.Progress
import seqexec.web.client.reusability._

/**
  * Progress bar divided in steps
  */
final case class DividedProgress(
  sections:             List[DividedProgress.Label],
  sectionTotal:         DividedProgress.Quantity,
  value:                DividedProgress.Quantity,
  indicating:           Boolean = false,
  progress:             Boolean = false,
  completeSectionColor: Option[SemanticColor] = None,
  ongoingSectionColor:  Option[SemanticColor] = None,
  progressCls:          Css
) extends ReactProps[DividedProgress](DividedProgress.component)

object DividedProgress {
  type Props = DividedProgress

  type Label    = String
  type Quantity = Int

  implicit val propsReuse: Reusability[Props] = Reusability.derive[DividedProgress]

  protected val component = ScalaComponent
    .builder[Props]("DividedProgress")
    .stateless
    .render_P { p =>
      val countSections = p.sections.length

      val sectionProgressStyles: List[Css] =
        // Length is 1 + (countSections - 2) + 1 = countSections
        SeqexecStyles.dividedProgressSectionLeft +:
          List.fill(countSections - 2)(SeqexecStyles.dividedProgressSectionMiddle) :+
          SeqexecStyles.dividedProgressSectionRight

      val completeSections = p.value / p.sectionTotal

      val sectionValuesAndColors: List[(Quantity, Option[SemanticColor])] =
        // Length is completeSections + 1 + (countSections - completeSections - 1) = countSections
        (List.fill(completeSections)((p.sectionTotal, p.completeSectionColor)) :+
          ((p.value % p.sectionTotal, p.ongoingSectionColor))) ++
          List.fill(countSections - completeSections - 1)((0, None))

      val sectionBarStyles: List[Css] =
        if (completeSections === 0)
          List.empty
        else
          SeqexecStyles.dividedProgressBarLeft +:
            List
              .fill(completeSections - 1)(SeqexecStyles.dividedProgressBarMiddle)
              .take(countSections - 2) :+
            SeqexecStyles.dividedProgressBarRight

      val sectionInfo =
        p.sections
          .zip(sectionValuesAndColors)
          .zip(sectionProgressStyles)
          .zip(
            sectionBarStyles.padTo(countSections, Css.Empty)
          ) // Due to padding, length = countSections

      <.span(
        SeqexecStyles.dividedProgress,
        sectionInfo.toTagMod {
          case (((label, (sectionValue, sectionColor)), sectionProgressStyle), sectionBarStyle) =>
            Progress(
              total      = p.sectionTotal,
              value      = sectionValue,
              indicating = p.indicating,
              progress   = p.progress,
              color      = sectionColor.orUndefined,
              clazz =
                p.progressCls |+| sectionProgressStyle |+| sectionBarStyle |+| SeqexecStyles.dividedProgressBar
            )(label)
        }
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
