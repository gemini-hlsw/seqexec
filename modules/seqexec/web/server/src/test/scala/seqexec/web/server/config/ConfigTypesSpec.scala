// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.config

import cats.tests.CatsSuite
import gem.enum.Site
import pureconfig._
import pureconfig.generic.auto._

/**
  * Tests of config classes
  */
final class ConfigTypesSpec extends CatsSuite {
  test("Test site config") {
    final case class TestConf(site: Site)

    ConfigSource.string("{ site: GS }").load[TestConf] shouldEqual TestConf(Site.GS).asRight
    ConfigSource.string("{ site: GN }").load[TestConf] shouldEqual TestConf(Site.GN).asRight
    ConfigSource.string("{ site: G }").load[TestConf].isLeft shouldBe(true)
  }
}
