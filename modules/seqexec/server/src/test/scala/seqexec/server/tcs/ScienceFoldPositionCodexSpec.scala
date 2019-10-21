// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import org.scalatest.matchers.should.Matchers._
import ScienceFoldPositionCodex._
import seqexec.server.EpicsCodex._
import gem.enum.LightSinkName.{Gmos, Gsaoi, Nifs, Niri_f32, F2}
import seqexec.server.tcs.TcsController.LightSource.{AO, GCAL, Sky}
import org.scalatest.flatspec.AnyFlatSpec

class ScienceFoldPositionCodexSpec extends AnyFlatSpec {

  private val invalid = "Invalid"
  private val parked = ("park-pos.", ScienceFold.Parked)
  private val ao2gmos3 = ("ao2gmos3", ScienceFold.Position(AO, Gmos, 3))
  private val gcal2nifs1 = ("gcal2nifs1", ScienceFold.Position(GCAL, Nifs, 1))
  private val gsaoi5 = ("gsaoi5", ScienceFold.Position(Sky, Gsaoi, 5))
  private val ao2niri32 = ("ao2nirif32p5", ScienceFold.Position(AO, Niri_f32, 5))
  private val f21 = ("f21", ScienceFold.Position(Sky, F2, 1))
  private val testVals = List(ao2gmos3, gcal2nifs1, gsaoi5, ao2niri32, f21)

  "ScienceFoldPositionCodex" should "properly decode EPICS strings into ScienceFold values" in {

    testVals.foreach{
      case (s, v) => decode[String, Option[ScienceFold]](s) shouldBe Some(v)
    }

    decode[String, Option[ScienceFold]](invalid) shouldBe None

    decode[String, Option[ScienceFold]](parked._1) shouldBe Some(parked._2)

  }
  it should "properly encode Position values into EPICS strings" in {

    decode[String, Option[ScienceFold]](invalid) shouldBe None

    testVals.foreach{
      case (s, v) => encode[ScienceFold.Position, String](v) shouldBe s
    }

  }

}
