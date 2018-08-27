// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import diode.{Action, Effect, NoAction}
import scala.concurrent.Future
import cats.Monoid
import scala.concurrent.ExecutionContext.Implicits.global

package handlers {
  trait Handlers {
    implicit def pfMonoid[A, B]: Monoid[PartialFunction[A, B]] = new Monoid[PartialFunction[A, B]] {
      override def empty = PartialFunction.empty[A, B]
      override def combine(x: PartialFunction[A, B], y: PartialFunction[A, B]): PartialFunction[A, B] = x.orElse(y)
    }
  }
}

package object handlers {
  val VoidEffect: Effect = Effect(Future(NoAction: Action))
}
