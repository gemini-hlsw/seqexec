// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package giapi.client

import cats.syntax.all._
import giapi.client.commands._
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Arbitrary._

trait GiapiArbitraries {
  implicit val configurationArb                         = Arbitrary {
    for {
      m <- arbitrary[Map[String, String]]
    } yield m
      .map { case (k, v) =>
        Configuration.single(k, v)
      }
      .toList
      .combineAll
  }

  implicit val configurationCogen: Cogen[Configuration] =
    Cogen[Map[String, String]].contramap {
      _.config.map { case (c, v) =>
        (c.getName, v)
      }
    }
}
