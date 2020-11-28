// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import scala.concurrent.duration.FiniteDuration

import cats.Eq
import org.http4s.Uri
import shapeless.tag.@@

trait GpiSettings
trait GhostSettings

/**
 * Configuration of the Seqexec Engine
 * @param odb Location of the odb server
 * @param dhsServer Location of the dhs server proxy
 * @param systemControl Control of the subsystems
 * @param odbNotifications Indicates if we notify the odb of sequence events
 * @param instForceError Used for testing to simulate errors
 * @param failAt At what step to fail if we simulate errors
 * @param odbQueuePollingInterval frequency to check the odb queue
 * @param gpiUrl URL for the GPI GMP
 * @param gpiGDS URL for GPI's GDS
 * @param ghostUrl URL for GHOST GMP
 * @param ghostGDS URL for GHOST's GDS
 * @param tops Used to select the top component for epics subsystems
 * @param epicsCaAddrList List of IPs for the epics subsystem
 * @param ioTimeout Timeout to listen for EPICS events
 * @param dhsTimeout Timeout for DHS operations
 */
final case class SeqexecEngineConfiguration(
  odb:                     Uri,
  dhsServer:               Uri,
  systemControl:           SystemsControlConfiguration,
  odbNotifications:        Boolean,
  instForceError:          Boolean,
  failAt:                  Int,
  odbQueuePollingInterval: FiniteDuration,
  gpiUrl:                  Uri @@ GpiSettings,
  gpiGDS:                  Uri @@ GpiSettings,
  ghostUrl:                Uri @@ GhostSettings,
  ghostGDS:                Uri @@ GhostSettings,
  tops:                    String,
  epicsCaAddrList:         Option[String],
  ioTimeout:               FiniteDuration,
  dhsTimeout:              FiniteDuration
)

object SeqexecEngineConfiguration {
  private implicit def taggedUriEq[A]: Eq[Uri @@ A] = Eq.by(x => x: Uri)

  implicit val eqSeqexecEngineConfiguration: Eq[SeqexecEngineConfiguration] =
    Eq.by(
      x =>
        (x.odb,
         x.dhsServer,
         x.systemControl,
         x.odbNotifications,
         x.instForceError,
         x.failAt,
         x.odbQueuePollingInterval,
         x.gpiUrl,
         x.gpiGDS,
         x.ghostUrl,
         x.ghostGDS,
         x.tops,
         x.epicsCaAddrList,
         x.ioTimeout,
         x.dhsTimeout)
    )

}
