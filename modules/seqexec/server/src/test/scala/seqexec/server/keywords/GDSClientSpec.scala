// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.tests.CatsSuite
import org.http4s.Uri._
import scala.xml.XML

@SuppressWarnings(Array("org.wartremover.warts.Throw"))
final class GDSClientSpec extends CatsSuite {
  test("GDSClient should reject bad responses") {
    val xml = XML.load(getClass.getResource("/gds-bad-resp.xml"))
    GDSClient.checkError(xml, uri("http://localhost:8888/xmlrpc")).isLeft shouldEqual true
  }
}
