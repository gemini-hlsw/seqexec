// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model.config

import cats.Eq
import cats.implicits._
import org.http4s.Uri
import scala.concurrent.duration.FiniteDuration
import shapeless.tag.@@

trait GpiSettings
trait GhostSettings

final case class SeqexecServerConfiguration(
                          odb:                     String,
                          dhsServer:               Uri,
                          systemControl:           SystemsControlConfiguration,
                          odbNotifications:        Boolean,
                          instForceError:          Boolean,
                          failAt:                  Int,
                          odbQueuePollingInterval: FiniteDuration,
                          gpiUrl:                  Uri @@ GpiSettings,
                          ghostUrl:                Uri @@ GhostSettings,
                          gpiGDS:                  Uri @@ GpiSettings,
                          ghostGDS:                Uri @@ GhostSettings)
                          // tops:                    String,
                          // caAddrList:              String,
                          // ioTimeout:               Duration)

object SeqexecServerConfiguration {
  private implicit def taggedUriEq[A]: Eq[Uri @@ A] = Eq.by(x => x: Uri)

  implicit val eqSeqexecServerConfiguration: Eq[SeqexecServerConfiguration] =
    Eq.by(x => (x.odb, x.dhsServer, x.systemControl, x.odbNotifications, x.instForceError, x.failAt, x.odbQueuePollingInterval, x.gpiUrl, x.ghostUrl, x.gpiGDS, x.ghostGDS))

}
