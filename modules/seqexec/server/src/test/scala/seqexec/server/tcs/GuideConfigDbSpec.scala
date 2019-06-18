// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.tcs

import org.scalatest.FlatSpec
import org.scalatest.Matchers._
import io.circe.parser._
import GuideConfigDb._
import cats.effect.IO
import cats.implicits._
import seqexec.model.enum._
import seqexec.model.TelescopeGuideConfig
import seqexec.server.altair.AltairController.Lgs
import squants.space.Millimeters

final class GuideConfigDbSpec extends FlatSpec {

  val rawJson1: String = """
  {
    "tcsGuide": {
      "m1Guide": {
        "on": true,
        "source": "PWFS1"
      },
      "m2Guide": {
        "on": true,
        "sources": ["PWFS1"],
        "comaOn": false
      },
      "mountGuideOn": true
    },
    "gaosGuide": null
  }
  """
  val guideConfig1: GuideConfig = GuideConfig(
    TelescopeGuideConfig(
      MountGuideOption.MountGuideOn,
      M1GuideConfig.M1GuideOn(M1Source.PWFS1),
      M2GuideConfig.M2GuideOn(ComaOption.ComaOff, Set(TipTiltSource.PWFS1))
    ),
    None
  )

  val rawJson2: String = """
  {
    "tcsGuide": {
      "m1Guide": {
        "on": true,
        "source": "PWFS1"
      },
      "m2Guide": {
        "on": true,
        "sources": ["PWFS1"],
        "comaOn": true
      },
      "mountGuideOn": false
    },
    "gaosGuide": {
      "altair": {
        "mode": "LGS",
        "aoOn": true,
        "strapOn": true,
        "sfoOn": true,
        "useOI": false,
        "useP1": false,
        "oiBlend": false,
        "aogsx": -5.0,
        "aogsy": 3.0
      }
    }
  }
  """
  val guideConfig2: GuideConfig = GuideConfig(
    TelescopeGuideConfig(
      MountGuideOption.MountGuideOff,
      M1GuideConfig.M1GuideOn(M1Source.PWFS1),
      M2GuideConfig.M2GuideOn(ComaOption.ComaOn, Set(TipTiltSource.PWFS1))
    ),
    Some(Left(Lgs(strap = true, sfo = true, starPos = (Millimeters(-5.0), Millimeters(3.0)))))
  )

  "GuideConfigDb" should "provide decoders" in {
    decode[GuideConfig](rawJson1) shouldBe Right(guideConfig1)
    decode[GuideConfig](rawJson2) shouldBe Right(guideConfig2)
  }

  it should "retrieve the same configuration that was set" in {
    val db = GuideConfigDb.newDb[IO]

    val ret = db.flatMap(x => x.set(guideConfig1) *> x.value).unsafeRunSync

    ret shouldBe guideConfig1
  }

}
