// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.components.sequence

package object steps {
  implicit class ControlButtonResolverOps[A](val a: A) extends AnyVal {
    def controlButtonsActive(implicit resolver: ControlButtonResolver[A]): Boolean =
      resolver.controlButtonsActive(a)
  }
}
