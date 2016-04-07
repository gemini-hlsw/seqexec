package edu.gemini.seqexec.web.client.components

import org.scalatest.{FlatSpec, Matchers}

class SeqexecStyleSpec extends FlatSpec with Matchers {
  "SeqexecStyles" should
    "render" in {
      SeqexecStyles.styles.size should be >= 0
    }
}
