// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import cats.effect.Sync
import cats.syntax.all._
import edu.gemini.epics.acm.CaService
import org.typelevel.log4cats.Logger
import seqexec.model.config.SeqexecEngineConfiguration

object CaServiceInit {
  // Ensure there is a valid way to init CaService either from
  // the configuration file or from the environment
  def caInit[F[_]](
    conf: SeqexecEngineConfiguration
  )(implicit F: Sync[F], L: Logger[F]): F[CaService] = {
    def setAddressList(a: String) =
      F.delay(CaService.setAddressList(a))

    L.info("Init EPICS but all subsystems in simulation")
      .unlessA(conf.systemControl.connectEpics) *>
      conf.epicsCaAddrList.map(setAddressList).getOrElse {
        F.delay(Option(System.getenv("EPICS_CA_ADDR_LIST"))).flatMap {
          case Some(a) => setAddressList(a).void
          case _       => F.raiseError[Unit](new RuntimeException("Cannot initialize EPICS subsystem"))
        }
      } *>
      F.delay {
        CaService.setReadRetries(conf.readRetries)
        CaService.setIOTimeout(java.time.Duration.ofMillis(conf.ioTimeout.toMillis))
      } *>
      F.delay(CaService.getInstance())
        .ensure(new Exception("Unable to start EPICS service."))(c => Option(c).isDefined)
  }
}
