package edu.gemini.seqexec

import scalaz._

package object shared {
  type TrySeq[A] = SeqFailure \/ A
}
