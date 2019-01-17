// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import gem.enum.Site
import java.time.LocalDate
import org.http4s.Uri
import scala.concurrent.duration.Duration
import shapeless.tag.@@
import giapi.client.gpi.GpiClient
import giapi.client.ghost.GhostClient

trait GpiSettings
trait GhostSettings

final case class Settings(site:                    Site,
                          odbHost:                 String,
                          date:                    LocalDate,
                          dhsURI:                  Uri,
                          dhsControl:              ControlStrategy,
                          f2Control:               ControlStrategy,
                          gcalControl:             ControlStrategy,
                          ghostControl:            ControlStrategy,
                          gmosControl:             ControlStrategy,
                          gnirsControl:            ControlStrategy,
                          gpiControl:              ControlStrategy,
                          gpiGdsControl:           ControlStrategy,
                          ghostGdsControl:         ControlStrategy,
                          gsaoiControl:            ControlStrategy,
                          gwsControl:              ControlStrategy,
                          nifsControl:             ControlStrategy,
                          niriControl:             ControlStrategy,
                          tcsControl:              ControlStrategy,
                          odbNotifications:        Boolean,
                          instForceError:          Boolean,
                          failAt:                  Int,
                          odbQueuePollingInterval: Duration,
                          gpi:                     GpiClient[cats.effect.IO],
                          ghost:                   GhostClient[cats.effect.IO],
                          gpiGDS:                  Uri @@ GpiSettings,
                          ghostGDS:                Uri @@ GhostSettings)
