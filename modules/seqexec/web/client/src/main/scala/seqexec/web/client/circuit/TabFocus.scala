// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.circuit

import cats.Eq
import cats.implicits._
import cats.data.NonEmptyList
import monocle.Getter
import seqexec.model._
import seqexec.web.client.model._

final case class TabFocus(
  canOperate:      Boolean,
  tabs:            NonEmptyList[Either[CalibrationQueueTabActive, AvailableTab]],
  defaultObserver: Observer
)

object TabFocus {
  implicit val eq: Eq[TabFocus] =
    Eq.by(x => (x.canOperate, x.tabs, x.defaultObserver))

  val tabFocusG: Getter[SeqexecAppRootModel, TabFocus] = {
    val getter = SeqexecAppRootModel.uiModel.composeGetter(
      (SeqexecUIModel.sequencesOnDisplay
        .composeGetter(SequencesOnDisplay.availableTabsG))
        .zip(SeqexecUIModel.defaultObserverG))
    ClientStatus.canOperateG.zip(getter) >>> {
      case (o, (t, ob)) =>
        TabFocus(o, t, ob)
    }
  }

}
