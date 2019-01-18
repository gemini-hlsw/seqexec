// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import gem.enum.Site
import giapi.client.Giapi
import java.time.LocalDate
import org.http4s.Uri
import scala.concurrent.duration.Duration
import shapeless.tag.@@

trait GpiSettings
trait GhostSettings

final case class Settings[F[_]](site:                    Site,
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
                                gpiGiapi:                Giapi[F] @@ GpiSettings,
                                ghostGiapi:              Giapi[F] @@ GhostSettings,
                                gpiGDS:                  Uri @@ GpiSettings,
                                ghostGDS:                Uri @@ GhostSettings)
