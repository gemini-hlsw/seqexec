// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import gem.enum._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{ FlatSpec, Matchers }
import scala.reflect.ClassTag

class EnumeratedSpec extends FlatSpec with Matchers with PropertyChecks {

  def checkEnumeration[A](
    implicit en: Enumerated[A],
             ct: ClassTag[A]
  ): Unit =
    s"Ordering for ${ct.runtimeClass.getName}" should "be canonical" in {
      val sorted   = en.all
      val shuffled = scala.util.Random.shuffle(sorted)
      shuffled.sorted(en.toOrdering) shouldEqual sorted
    }

  // Check a handful of enums.
  checkEnumeration[EventType]
  checkEnumeration[F2Disperser]
  checkEnumeration[GcalArc]
  checkEnumeration[GmosAdc]
  checkEnumeration[Half]
  checkEnumeration[Instrument]
  checkEnumeration[MosPreImaging]
  checkEnumeration[ProgramRole]
  checkEnumeration[ProgramType]
  checkEnumeration[Site]
  checkEnumeration[SmartGcalType]
  checkEnumeration[StepType]

}
