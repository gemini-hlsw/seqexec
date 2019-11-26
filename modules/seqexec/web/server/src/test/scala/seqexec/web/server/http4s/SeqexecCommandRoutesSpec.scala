// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.IO
import cats.tests.CatsSuite
import gem.arb.ArbEnumerated._
import org.http4s._
import org.scalamock.scalatest.MockFactory
import org.http4s.Uri.uri
import seqexec.server._
import seqexec.web.server.http4s.encoder._
import seqexec.model.enum._

class SeqexecCommandRoutesSpec
    extends CatsSuite
    with MockFactory
    with ClientBooEncoders
    with TestRoutes {
  test("update water vapor") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      WaterVapor.WaterVaporEnumerated.all.foreach(
        wv =>
          (engine.setWaterVapor _)
            .expects(*, wv, *)
            .anyNumberOfTimes
            .returning(IO.unit)
      )
    }
    forAll { (wv: WaterVapor) =>
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri("/wv"))
            .addCookie("token", t)
            .withEntity(wv)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync === Some(s"Set water vapor to $wv"))
    }
  }
  test("update image quality") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      ImageQuality.ImageQualityEnumerated.all.foreach(
        wv =>
          (engine.setImageQuality _)
            .expects(*, wv, *)
            .anyNumberOfTimes
            .returning(IO.unit)
      )
    }
    forAll { (iq: ImageQuality) =>
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri("/iq"))
            .addCookie("token", t)
            .withEntity(iq)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync === Some(s"Set image quality to $iq"))
    }
  }
  test("update sky background") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      SkyBackground.SkyBackgroundEnumerated.all.foreach(
        wv =>
          (engine.setSkyBackground _)
            .expects(*, wv, *)
            .anyNumberOfTimes
            .returning(IO.unit)
      )
    }
    forAll { (sb: SkyBackground) =>
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri("/sb"))
            .addCookie("token", t)
            .withEntity(sb)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync === Some(s"Set sky background to $sb"))
    }
  }
  test("update cloud cover") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      CloudCover.CloudCoverEnumerated.all.foreach(
        wv =>
          (engine.setCloudCover _)
            .expects(*, wv, *)
            .anyNumberOfTimes
            .returning(IO.unit)
      )
    }
    forAll { (cc: CloudCover) =>
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri("/cc"))
            .addCookie("token", t)
            .withEntity(cc)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync === Some(s"Set cloud cover to $cc"))
    }
  }

}
