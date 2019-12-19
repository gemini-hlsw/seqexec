// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import seqexec.server.tcs.TcsController._
import seqexec.server.EpicsCommand
import seqexec.server.tcs.TcsEpics.{ProbeFollowCmd, ProbeGuideCmd}

final case class GuideControl[F[_]](subs: Subsystem,
                                    parkCmd: EpicsCommand[F],
                                    nodChopGuideCmd: ProbeGuideCmd[F],
                                    followCmd: ProbeFollowCmd[F])
