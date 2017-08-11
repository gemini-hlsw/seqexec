// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.components

import org.scalatest.{FlatSpec, Matchers}

class SeqexecStyleSpec extends FlatSpec with Matchers {
  "SeqexecStyles" should
    "render" in {
      SeqexecStyles.styles.size should be >= 0
    }
}
