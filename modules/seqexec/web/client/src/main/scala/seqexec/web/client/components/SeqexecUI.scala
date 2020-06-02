// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components

import diode.ModelRO
import cats.implicits._
import cats.effect.Sync
import monocle.Prism
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.extra.router._
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.MonocleReact._
import gem.Observation
import gem.enum.Site
import scala.scalajs.js.timers.SetTimeoutHandle
import seqexec.web.client.circuit.SeqexecCircuit
import seqexec.web.client.actions.WSConnect
import seqexec.web.client.model.Pages._
import seqexec.web.client.model.ModelOps._
import seqexec.web.client.actions.{NavigateSilentTo, RequestSoundEcho}
import seqexec.model.enum.Instrument

/**
  * UI Router
  */
object SeqexecUI {
  private def pageTitle(site: Site)(p: SeqexecPages): String = p match {
    case SequenceConfigPage(_, id, _) => s"Seqexec - ${id.format}"
    case SequencePage(_, id, _)       => s"Seqexec - ${id.format}"
    case PreviewPage(_, id, _)        => s"Seqexec - ${id.format}"
    case PreviewConfigPage(_, id, _)  => s"Seqexec - ${id.format}"
    case CalibrationQueuePage         => s"Seqexec - Daycal queue"
    case _                            => s"Seqexec - ${site.shortName}"
  }

  // Prism from url params to config page
  private def configPageP(instrumentNames: Map[String, Instrument]): Prism[(String, String, Int), SequenceConfigPage] =
    Prism[(String, String, Int), SequenceConfigPage] {
      case (i, s, step) => (instrumentNames.get(i), Observation.Id.fromString(s)).mapN(SequenceConfigPage(_, _, step))
    } {
      p => (p.instrument.show, p.obsId.format, p.step)
    }

  // Prism from url params to sequence page
  private def sequencePageSP(instrumentNames: Map[String, Instrument]): Prism[(String, String, Option[Int]), SequencePage] =
    Prism[(String, String, Option[Int]), SequencePage] {
      case (i, s, st) => (instrumentNames.get(i), Observation.Id.fromString(s)).mapN(SequencePage(_, _, StepIdDisplayed(st.foldMap(_ - 1))))
    } {
      p => (p.instrument.show, p.obsId.format, (p.step.step + 1).some)
    }

  // Prism from url params to the preview page to a given step
  private def previewPageSP(instrumentNames: Map[String, Instrument]): Prism[(String, String, Option[Int]), PreviewPage] =
    Prism[(String, String, Option[Int]), PreviewPage] {
      case (i, s, st) => (instrumentNames.get(i), Observation.Id.fromString(s)).mapN(PreviewPage(_, _, StepIdDisplayed(st.foldMap(_ - 1))))
    } {
      p => (p.instrument.show, p.obsId.format, (p.step.step + 1).some)
    }

  // Prism from url params to the preview page with config
  private def previewConfigPageP(instrumentNames: Map[String, Instrument]): Prism[(String, String, Int), PreviewConfigPage] =
    Prism[(String, String, Int), PreviewConfigPage] {
      case (i, s, step) => (instrumentNames.get(i), Observation.Id.fromString(s)).mapN(PreviewConfigPage(_, _, step))
    } {
      p => (p.instrument.show, p.obsId.format, p.step)
    }

  def router[F[_]](site: Site)(implicit F: Sync[F]): F[Router[SeqexecPages]] = {
    val instrumentNames = site.instruments.map(i => (i.show, i)).toList.toMap

    val routerConfig = RouterConfigDsl[SeqexecPages].buildConfig { dsl =>
      import dsl._

      (emptyRule
      | staticRoute(root, Root) ~> renderR(r => SeqexecMain(site, r))
      | staticRoute("/soundtest", SoundTest) ~> renderR(r => SeqexecMain(site, r))
      | staticRoute("/daycal", CalibrationQueuePage) ~> renderR(r => SeqexecMain(site, r))
      | dynamicRouteCT(("/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+") ~ "/configuration/" ~ int)
        .pmapL(configPageP(instrumentNames))) ~> dynRenderR((_: SequenceConfigPage, r) => SeqexecMain(site, r))
      | dynamicRouteCT(("/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+") ~ ("/step/" ~ int).option)
        .pmapL(sequencePageSP(instrumentNames))) ~> dynRenderR((_: SequencePage, r) => SeqexecMain(site, r))
      | dynamicRouteCT(("/preview/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+") ~ ("/step/" ~ int).option)
        .pmapL(previewPageSP(instrumentNames))) ~> dynRenderR((_: PreviewPage, r) => SeqexecMain(site, r))
      | dynamicRouteCT(("/preview/" ~ string("[a-zA-Z0-9-]+") ~ "/" ~ string("[a-zA-Z0-9-]+") ~ "/configuration/" ~ int)
        .pmapL(previewConfigPageP(instrumentNames))) ~> dynRenderR((_: PreviewConfigPage, r) => SeqexecMain(site, r))
      )
        .notFound(redirectToPage(Root)(SetRouteVia.HistoryPush))
        // Runtime verification that all pages are routed
        .verify(Root, List(SoundTest, CalibrationQueuePage): _*)
        .onPostRender((_, next) =>
          Callback.when(next === SoundTest)(SeqexecCircuit.dispatchCB(RequestSoundEcho)) *>
          Callback.when(next =!= SeqexecCircuit.zoom(_.uiModel.navLocation).value)(SeqexecCircuit.dispatchCB(NavigateSilentTo(next))))
        .renderWith { case (_, r) => <.div(r.render()) }
        .setTitle(pageTitle(site))
        .logToConsole
    }

    def navigated(routerLogic: RouterLogic[SeqexecPages, Unit], page: ModelRO[SeqexecPages]): SetTimeoutHandle = {
      scalajs.js.timers.setTimeout(0)(routerLogic.ctl.set(page.value).runNow())
    }

    for {
      r                     <- F.delay(Router.componentAndLogic(BaseUrl.fromWindowOrigin, routerConfig))
      (router, routerLogic) = r
      // subscribe to navigation changes
      _                     <- F.delay(SeqexecCircuit.subscribe(SeqexecCircuit.zoom(_.uiModel.navLocation))(x => {navigated(routerLogic, x);()}))
        // Initiate the WebSocket connection
      _                     <- F.delay(SeqexecCircuit.dispatch(WSConnect(0)))
    } yield router
  }

}
