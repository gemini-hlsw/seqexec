// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.web.client.model

import scalaz.Monoid

object Handlers {
  implicit def pfMonoid[A, B]: Monoid[PartialFunction[A, B]] = new Monoid[PartialFunction[A, B]] {
    def zero = PartialFunction.empty[A, B]
    def append(pf1: PartialFunction[A, B], pf2: => PartialFunction[A, B]) = pf1.orElse(pf2)
  }
}
