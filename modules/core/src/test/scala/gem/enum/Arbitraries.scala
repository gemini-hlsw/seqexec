package gem.enum

import org.scalacheck._
import org.scalacheck.Gen._

//import scalaz._, Scalaz._


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
