// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import cats.effect.IO
import cats.tests.CatsSuite
import gem.Observation
import gem.arb.ArbObservation._
import gem.arb.ArbEnumerated.{arbEnumerated => oldArbEnumerated}
import lucuma.core.util.arb.ArbEnumerated._
import java.net.URLEncoder
import org.http4s._
import org.scalamock.scalatest.MockFactory
import org.http4s.Uri.uri
import scala.math.abs
import seqexec.server._
import seqexec.web.server.http4s.encoder._
import seqexec.model.ClientId
import seqexec.model.enum._
import seqexec.model.arb.ArbClientId._

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
            .anyNumberOfTimes()
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
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync() === Some(s"Set water vapor to $wv"))
    }
  }

  test("update image quality") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      ImageQuality.ImageQualityEnumerated.all.foreach(
        wv =>
          (engine.setImageQuality _)
            .expects(*, wv, *)
            .anyNumberOfTimes()
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
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync() === Some(s"Set image quality to $iq"))
    }
  }

  test("update sky background") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      SkyBackground.SkyBackgroundEnumerated.all.foreach(
        wv =>
          (engine.setSkyBackground _)
            .expects(*, wv, *)
            .anyNumberOfTimes()
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
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync() === Some(s"Set sky background to $sb"))
    }
  }

  test("update cloud cover") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      CloudCover.CloudCoverEnumerated.all.foreach(
        wv =>
          (engine.setCloudCover _)
            .expects(*, wv, *)
            .anyNumberOfTimes()
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
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync() === Some(s"Set cloud cover to $cc"))
    }
  }

  test("start sequence") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.start _)
        .expects(*, *, *, *, *)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, clientId: ClientId) =>
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST,
                      uri = Uri.unsafeFromString(
                        s"/${obsId.format}/start/${clientId.self}"
                      ))
            .addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync() === Some(s"Started sequence ${obsId.format}"))
    }
  }

  test("start sequence from") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.startFrom _)
        .expects(*, *, *, *, *)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, step: Int, clientId: ClientId) =>
      val startFrom = abs(step / 2) + 1
      val uri = Uri.unsafeFromString(
        s"/${obsId.format}/$startFrom/startFrom/${clientId.self}"
      )
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Started sequence ${obsId.format} from step $startFrom"
        )
      )
    }
  }

  test("pause sequence") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.requestPause _)
        .expects(*, *, *)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id) =>
      val uri = Uri.unsafeFromString(s"/${obsId.format}/pause")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync() === Some(s"Pause sequence ${obsId.format}"))
    }
  }

  test("cancelpause sequence") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.requestCancelPause _)
        .expects(*, *, *)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id) =>
      val uri = Uri.unsafeFromString(s"/${obsId.format}/cancelpause")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(b.unsafeRunSync() === Some(s"Cancel Pause sequence ${obsId.format}"))
    }
  }

  test("set breakpoint") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.setBreakpoint _)
        .expects(*, *, *, *, *)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, step: Int, set: Boolean) =>
      val toSet = abs(step / 2) + 1
      val uri   = Uri.unsafeFromString(s"/${obsId.format}/$toSet/breakpoint/$set")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Set breakpoint in step $toSet of sequence ${obsId.format}"
        )
      )
    }
  }

  test("set skip") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.setSkipMark _)
        .expects(*, *, *, *, *)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, step: Int, set: Boolean) =>
      val toSet = abs(step / 2) + 1
      val uri   = Uri.unsafeFromString(s"/${obsId.format}/$toSet/skip/$set")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Set skip mark in step $toSet of sequence ${obsId.format}"
        )
      )
    }
  }

  test("stop sequence") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.stopObserve _)
        .expects(*, *, false)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, st: Int) =>
      val step = abs(st / 2) + 1
      val uri  = Uri.unsafeFromString(s"/${obsId.format}/$step/stop")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Stop requested for ${obsId.format} on step $step"
        )
      )
    }
  }

  test("stop sequence gracefully") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.stopObserve _)
        .expects(*, *, true)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, st: Int) =>
      val step = abs(st / 2) + 1
      val uri  = Uri.unsafeFromString(s"/${obsId.format}/$step/stopGracefully")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Stop gracefully requested for ${obsId.format} on step $step"
        )
      )
    }
  }

  test("abort sequence") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.abortObserve _)
        .expects(*, *)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, st: Int) =>
      val step = abs(st / 2) + 1
      val uri  = Uri.unsafeFromString(s"/${obsId.format}/$step/abort")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Abort requested for ${obsId.format} on step $step"
        )
      )
    }
  }

  test("pause obs sequence") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.pauseObserve _)
        .expects(*, *, false)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, st: Int) =>
      val step = abs(st / 2) + 1
      val uri  = Uri.unsafeFromString(s"/${obsId.format}/$step/pauseObs")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Pause observation requested for ${obsId.format} on step $step"
        )
      )
    }
  }

  test("pause obs gracefully") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.pauseObserve _)
        .expects(*, *, true)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, st: Int) =>
      val step = abs(st / 2) + 1
      val uri =
        Uri.unsafeFromString(s"/${obsId.format}/$step/pauseObsGracefully")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Pause observation gracefully requested for ${obsId.format} on step $step"
        )
      )
    }
  }

  test("resume obs") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.resumeObserve _)
        .expects(*, *)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, st: Int) =>
      val step = abs(st / 2) + 1
      val uri  = Uri.unsafeFromString(s"/${obsId.format}/$step/resumeObs")
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Resume observation requested for ${obsId.format} on step $step"
        )
      )
    }
  }

  test("operator") {
    val engine = mock[SeqexecEngine[IO]]
    inAnyOrder {
      (engine.setObserver _)
        .expects(*, *, *, *)
        .anyNumberOfTimes()
        .returning(IO.unit)
    }
    forAll { (obsId: Observation.Id, obs: String) =>
      val uri = Uri.unsafeFromString(
        s"/${obsId.format}/observer/${URLEncoder.encode(obs, "UTF-8")}"
      )
      val (s, b) = (for {
        s <- commandRoutes(engine)
        t <- newLoginToken
        l <- s(
          Request[IO](method = Method.POST, uri = uri).addCookie("token", t)
        ).value
      } yield (l.map(_.status), l.map(_.as[String]).sequence)).unsafeRunSync()
      assert(s === Some(Status.Ok))
      assert(
        b.unsafeRunSync() === Some(
          s"Set observer name to '${obs}' for sequence ${obsId.format}"
        )
      )
    }
  }
}
