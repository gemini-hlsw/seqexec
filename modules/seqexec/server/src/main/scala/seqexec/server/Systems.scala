// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect._
import cats.implicits._
import edu.gemini.spModel.core.Peer
import gem.enum.Site
import io.chrisdavenport.log4cats.Logger
import giapi.client.ghost.GhostClient
import giapi.client.gpi.GpiClient
import org.http4s.client.Client
import seqexec.server.altair._
import seqexec.server.flamingos2._
import seqexec.server.keywords._
import seqexec.server.gpi._
import seqexec.server.gmos._
import seqexec.server.gsaoi._
import seqexec.server.ghost._
import seqexec.server.gcal._
import seqexec.server.gems._
import seqexec.server.tcs._
import seqexec.server.gnirs._
import seqexec.server.niri._
import seqexec.server.nifs._

final case class Systems[F[_]](
  odb:        OdbProxy[F],
  dhs:        DhsClient[F],
  tcsSouth:   TcsSouthController[F],
  tcsNorth:   TcsNorthController[F],
  gcal:       GcalController[F],
  flamingos2: Flamingos2Controller[F],
  gmosSouth:  GmosSouthController[F],
  gmosNorth:  GmosNorthController[F],
  gnirs:      GnirsController[F],
  gsaoi:      GsaoiController[F],
  gpi:        GpiController[F],
  ghost:      GhostController[F],
  niri:       NiriController[F],
  nifs:       NifsController[F],
  altair:     AltairController[F],
  gems:       GemsController[F],
  guideDb:    GuideConfigDb[F]
)

object Systems {
  def odbProxy[F[_]: Sync: Logger](settings: Settings): OdbProxy[F] = OdbProxy[F](new Peer(settings.odbHost, 8443, null),
    if (settings.odbNotifications) OdbProxy.OdbCommandsImpl[F](new Peer(settings.odbHost, 8442, null))
    else new OdbProxy.DummyOdbCommands[F])

  def dhs[F[_]: Effect: Timer: Logger](settings: Settings, httpClient: Client[F]): DhsClient[F] =
    if (settings.dhsControl.command) DhsClientHttp[F](httpClient, settings.dhsURI)
    else                             DhsClientSim.unsafeApply(settings.date)

  // TODO make instruments controllers generalized on F
  def gcal(settings: Settings)(implicit L: Logger[IO]): IO[GcalController[IO]] =
    if (settings.gcalControl.command) GcalControllerEpics(GcalEpics.instance).pure[IO]
    else                              GcalControllerSim[IO].pure[IO]

  def tcsSouth(settings: Settings, gcdb: GuideConfigDb[IO])(implicit L: Logger[IO]): IO[TcsSouthController[IO]] =
    if (settings.tcsControl.command && settings.site === Site.GS) TcsSouthControllerEpics(gcdb).pure[IO]
    else                                                          TcsSouthControllerSim[IO].pure[IO]

  def tcsNorth(settings: Settings)(implicit L: Logger[IO]): IO[TcsNorthController[IO]] =
    if (settings.tcsControl.command && settings.site === Site.GN) TcsNorthControllerEpics().pure[IO]
    else                                                          TcsNorthControllerSim[IO].pure[IO]

  def altair(settings: Settings)(implicit L: Logger[IO]): IO[AltairController[IO]] =
    if (settings.altairControl.command && settings.tcsControl.command) AltairControllerEpics.pure[IO]
    else                                                               AltairControllerSim[IO].pure[IO]

  def gems(settings: Settings, gsaoiController: GsaoiGuider[IO])(implicit L: Logger[IO]): IO[GemsController[IO]] =
    if (settings.gemsControl.command && settings.tcsControl.command) GemsControllerEpics(GemsEpics.instance, gsaoiController).pure[IO]
    else                                                             GemsControllerSim[IO].pure[IO]

  def gsaoi(settings: Settings)(implicit T: Timer[IO], L: Logger[IO]): IO[GsaoiFullHandler[IO]] =
    if (settings.gsaoiControl.command) GsaoiControllerEpics().pure[IO]
    else                               GsaoiControllerSim[IO]

  def gnirs(settings: Settings)(implicit T: Timer[IO], L: Logger[IO]): IO[GnirsController[IO]] =
    if (settings.gnirsControl.command) GnirsControllerEpics().pure[IO]
    else                               GnirsControllerSim[IO]

  def niri(settings: Settings)(implicit T: Timer[IO], L: Logger[IO]): IO[NiriController[IO]] =
    if (settings.niriControl.command) NiriControllerEpics().pure[IO]
    else                              NiriControllerSim[IO]

  def nifs(settings: Settings)(implicit T: Timer[IO], L: Logger[IO]): IO[NifsController[IO]] =
    if (settings.nifsControl.command) NifsControllerEpics().pure[IO]
    else                              NifsControllerSim[IO]

  def gmosSouth(settings: Settings)(implicit T: Timer[IO], L: Logger[IO]): IO[GmosSouthController[IO]] =
    if (settings.gmosControl.command) GmosSouthControllerEpics().pure[IO]
    else                              GmosControllerSim.south[IO]

  def gmosNorth(settings: Settings)(implicit T: Timer[IO], L: Logger[IO]): IO[GmosNorthController[IO]] =
    if (settings.gmosControl.command) GmosNorthControllerEpics().pure[IO]
    else                              GmosControllerSim.north[IO]

  def flamingos2(settings: Settings)(implicit T: Timer[IO], L: Logger[IO]): IO[Flamingos2Controller[IO]] =
    if (settings.f2Control.command) Flamingos2ControllerEpics[IO](Flamingos2Epics.instance).pure[IO]
    else if (settings.instForceError) Flamingos2ControllerSimBad[IO](settings.failAt)
    else Flamingos2ControllerSim[IO]

  def gpi[F[_]: ConcurrentEffect: Timer](settings: Settings, httpClient: Client[F]): Resource[F, GpiController[F]] = {
    def gpiClient(settings: Settings): Resource[F, GpiClient[F]] =
      if (settings.gpiControl.command) GpiClient.gpiClient[F](settings.gpiUrl.renderString, GpiStatusApply.statusesToMonitor)
      else                             GpiClient.simulatedGpiClient[F]

    def gpiGDS(settings: Settings, httpClient: Client[F]): Resource[F, GdsClient[F]] =
      Resource.pure(GdsClient(
        if (settings.gpiGdsControl.command) httpClient else GdsClient.alwaysOkClient[F],
        settings.gpiGDS))

    (gpiClient(settings), gpiGDS(settings, httpClient)).mapN(GpiController(_, _))
  }

  def ghost[F[_]: ConcurrentEffect: Timer](settings: Settings, httpClient: Client[F]): Resource[F, GhostController[F]] = {
    def ghostClient(settings: Settings): Resource[F, GhostClient[F]] =
      if (settings.ghostControl.command) GhostClient.ghostClient[F](settings.ghostUrl.renderString)
       else                              GhostClient.simulatedGhostClient

    def ghostGDS(settings: Settings, httpClient: Client[F]): Resource[F, GdsClient[F]] =
      Resource.pure(GdsClient(
        if (settings.ghostGdsControl.command) httpClient else GdsClient.alwaysOkClient[F],
        settings.ghostGDS))

    (ghostClient(settings), ghostGDS(settings, httpClient)).mapN(GhostController(_, _))
  }

  def build(httpClient: Client[IO], settings: Settings)(implicit T: Timer[IO], L: Logger[IO], C: ContextShift[IO]): Resource[IO, Systems[IO]] = {
    for {
      odbProxy        <- Resource.pure[IO, OdbProxy[IO]](odbProxy[IO](settings))
      dhsClient       <- Resource.pure[IO, DhsClient[IO]](dhs[IO](settings, httpClient))
      gcdb            <- Resource.liftF(GuideConfigDb.newDb[IO])
      gcalController  <- Resource.liftF(gcal(settings))
      tcsGS           <- Resource.liftF(tcsSouth(settings, gcdb))
      tcsGN           <- Resource.liftF(tcsNorth(settings))
      altair          <- Resource.liftF(altair(settings))
      gsaoiController <- Resource.liftF(gsaoi(settings))
      gems            <- Resource.liftF(gems(settings, gsaoiController))
      gnirsController <- Resource.liftF(gnirs(settings))
      f2Controller    <- Resource.liftF(flamingos2(settings))
      niriController  <- Resource.liftF(niri(settings))
      nifsController  <- Resource.liftF(nifs(settings))
      gmosSController <- Resource.liftF(gmosSouth(settings))
      gmosNController <- Resource.liftF(gmosNorth(settings))
      gpiController   <- gpi[IO](settings, httpClient)
      ghostController <- ghost[IO](settings, httpClient)
    } yield
      Systems[IO](
        odbProxy,
        dhsClient,
        tcsGS,
        tcsGN,
        gcalController,
        f2Controller,
        gmosSController,
        gmosNController,
        gnirsController,
        gsaoiController,
        gpiController,
        ghostController,
        niriController,
        nifsController,
        altair,
        gems,
        gcdb
      )

  }
}
