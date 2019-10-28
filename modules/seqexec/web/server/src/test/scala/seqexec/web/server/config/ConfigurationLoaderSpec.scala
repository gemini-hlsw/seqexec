// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.config

import cats.tests.CatsSuite
import cats.effect.IO
import gem.enum.Site
import java.nio.file.Paths
import org.http4s.Uri
import org.http4s.Uri._
import pureconfig._
import scala.concurrent.duration._
import seqexec.model.config._
import shapeless.tag

class ConfigurationLoaderSpec extends CatsSuite {
  val gcal =
    SmartGcalConfiguration(uri("gsodbtest.gemini.edu"), Paths.get("/tmp/smartgcal"))
  val tls  = TLSConfig(Paths.get("file.jks"), "key", "cert")
  val auth  = AuthenticationConfig(2.hour, "SeqexecToken", "somekey", false, List(uri("ldap://sbfdc-wv1.gemini.edu:3268")))
  val ws  = WebServerConfiguration("0.0.0.0", 7070, 7071, "localhost", Some(tls))
  val server  = SeqexecEngineConfiguration(
    uri("localhost"),
    uri("http://cpodhsxx:9090/axis2/services/dhs/images"),
    SystemsControlConfiguration(
      altair =   ControlStrategy.Simulated,
      gems =     ControlStrategy.Simulated,
      dhs =      ControlStrategy.Simulated,
      f2 =       ControlStrategy.Simulated,
      gcal =     ControlStrategy.Simulated,
      gmos =     ControlStrategy.Simulated,
      gnirs =    ControlStrategy.Simulated,
      gpi =      ControlStrategy.Simulated,
      gpiGds =   ControlStrategy.Simulated,
      ghost =    ControlStrategy.Simulated,
      ghostGds = ControlStrategy.Simulated,
      gsaoi =    ControlStrategy.Simulated,
      gws =      ControlStrategy.Simulated,
      nifs =     ControlStrategy.Simulated,
      niri =     ControlStrategy.Simulated,
      tcs =      ControlStrategy.Simulated),
    true,
    false,
    2,
    3.seconds,
    tag[GpiSettings][Uri](uri("vm://gpi?marshal=false&broker.persistent=false")),
    tag[GpiSettings][Uri](uri("http://localhost:8888/xmlrpc")),
    tag[GhostSettings][Uri](uri("vm://ghost?marshal=false&broker.persistent=false")),
    tag[GhostSettings][Uri](uri("http://localhost:8888/xmlrpc")),
    "tcs=tcs:, ao=ao:, gm=gm:, gc=gc:, gw=ws:, m2=m2:, oiwfs=oiwfs:, ag=ag:, f2=f2:, gsaoi=gsaoi:, aom=aom:, myst=myst:, rtc=rtc:",
    Some("127.0.0.1"),
    5.seconds)
  val ref = SeqexecConfiguration(Site.GS, Mode.Development, server, ws, gcal, auth)

  test("read config") {
    assert(
      loadConfiguration[IO](ConfigSource.resources("app.conf")).unsafeRunSync === ref
    )
  }

}
