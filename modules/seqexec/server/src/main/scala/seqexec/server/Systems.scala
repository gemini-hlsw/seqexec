// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server

import seqexec.server.altair.AltairController
import seqexec.server.flamingos2.Flamingos2Controller
import seqexec.server.keywords._
import seqexec.server.gpi.GpiController
import seqexec.server.gmos.GmosController
import seqexec.server.ghost.GhostController
import seqexec.server.gcal.GcalController
import seqexec.server.tcs.{GuideConfigDb, TcsController}
import seqexec.server.gnirs.GnirsController
import seqexec.server.niri.NiriController
import seqexec.server.nifs.NifsController

final case class Systems[F[_]](
  odb:        OdbProxy[F],
  dhs:        DhsClient[F],
  tcs:        TcsController,
  gcal:       GcalController,
  flamingos2: Flamingos2Controller,
  gmosSouth:  GmosController.GmosSouthController,
  gmosNorth:  GmosController.GmosNorthController,
  gnirs:      GnirsController,
  gpi:        GpiController[F],
  ghost:      GhostController[F],
  niri:       NiriController,
  nifs:       NifsController[F],
  altair:     AltairController[F],
  guideDb:    GuideConfigDb[F]
)
