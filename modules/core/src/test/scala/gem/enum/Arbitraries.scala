// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.enum

import org.scalacheck._
import org.scalacheck.Gen._


trait Arbitraries {

  private def pick[A](as: List[A]): Arbitrary[A] =
    Arbitrary(oneOf(as))


  // Flamingos2

  implicit val arbF2Disperser   = pick(F2Disperser.all  )
  implicit val arbF2Filter      = pick(F2Filter.all     )
  implicit val arbF2FpUnit      = pick(F2FpUnit.all     )
  implicit val arbF2LyotWheel   = pick(F2LyotWheel.all  )
  implicit val arbF2ReadMode    = pick(F2ReadMode.all   )
  implicit val arbF2WindowCover = pick(F2WindowCover.all)


  // Gmos

  implicit val arbGmosAdc             = pick(GmosAdc.all            )
  implicit val arbGmosAmpCount        = pick(GmosAmpCount.all       )
  implicit val arbGmosAmpGain         = pick(GmosAmpGain.all        )
  implicit val arbGmosAmpReadMode     = pick(GmosAmpReadMode.all    )
  implicit val arbGmosBinning         = pick(GmosBinning.all        )
  implicit val arbGmosBuiltinRoi      = pick(GmosBuiltinRoi.all     )
  implicit val arbGmosCustomSlitWidth = pick(GmosCustomSlitWidth.all)
  implicit val arbGmosDetector        = pick(GmosDetector.all       )
  implicit val arbGmosDisperserOrder  = pick(GmosDisperserOrder.all )
  implicit val arbGmosDtax            = pick(GmosDtax.all           )
  implicit val arbGmosNorthDisperser  = pick(GmosNorthDisperser.all )
  implicit val arbGmosNorthFilter     = pick(GmosNorthFilter.all    )
  implicit val arbGmosNorthFpu        = pick(GmosNorthFpu.all       )
  implicit val arbGmosNorthStageMode  = pick(GmosNorthStageMode.all )
  implicit val arbGmosSouthDisperser  = pick(GmosSouthDisperser.all )
  implicit val arbGmosSouthFilter     = pick(GmosSouthFilter.all    )
  implicit val arbGmosSouthFpu        = pick(GmosSouthFpu.all       )
  implicit val arbGmosSouthStageMode  = pick(GmosSouthStageMode.all )


  // Gcal

  implicit val arbGcalArc          = pick(GcalArc.all         )
  implicit val arbGcalBaselineType = pick(GcalBaselineType.all)
  implicit val arbGcalContinuum    = pick(GcalContinuum.all   )
  implicit val arbGcalDiffuser     = pick(GcalDiffuser.all    )
  implicit val arbGcalFilter       = pick(GcalFilter.all      )
  implicit val arbGcalLampType     = pick(GcalLampType.all    )
  implicit val arbGcalShutter      = pick(GcalShutter.all     )


  // General

  implicit val arbInstrument    = pick(Instrument.all   )
  implicit val arbProgramRole   = pick(ProgramRole.all  )
  implicit val arbProgramType   = pick(ProgramType.all  )
  implicit val arbSite          = pick(Site.all         )
  implicit val arbSmartGcalType = pick(SmartGcalType.all)
  implicit val arbStepType      = pick(StepType.all     )

}
