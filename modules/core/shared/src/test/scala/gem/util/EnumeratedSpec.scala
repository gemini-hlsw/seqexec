// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package util

import gem.arb._
import gem.enum._
import cats.tests.CatsSuite
import cats.kernel.laws.discipline._
import scala.reflect.ClassTag

final class EnumeratedSpec extends CatsSuite {
  import ArbEnumerated._

  def checkEnumeration[A](
    implicit en: Enumerated[A],
             ct: ClassTag[A]
  ): Unit = {
    val name = ct.runtimeClass.getSimpleName
    checkAll(name, OrderTests[A].order)
    test(s"$name.enumerated.canonical") {
      val sorted   = en.all
      val shuffled = scala.util.Random.shuffle(sorted)
      shuffled.sorted(en.toOrdering) shouldEqual sorted
    }
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
