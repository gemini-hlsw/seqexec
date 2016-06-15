package edu.gemini.seqexec.web.common

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}
import upickle.default._
import LogMessage._

class LogMessagePicklingSpec extends FlatSpec with Matchers with PropertyChecks with ArbitrariesWebCommon {
  "LogMessage" should "upickle/depickle" in {
    forAll { (a: LogMessage) =>
      read[LogMessage](write(a)) shouldEqual a
    }
  }
}
