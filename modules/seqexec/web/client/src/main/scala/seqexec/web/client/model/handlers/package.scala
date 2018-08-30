// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.Monoid
import diode.{Action, ActionHandler, ActionResult, Effect, NoAction}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

package handlers {
  trait Handlers[M, T] { this: ActionHandler[M, T] =>
    implicit def pfMonoid[A, B]: Monoid[PartialFunction[A, B]] = new Monoid[PartialFunction[A, B]] {
      override def empty = PartialFunction.empty[A, B]
      override def combine(x: PartialFunction[A, B], y: PartialFunction[A, B]): PartialFunction[A, B] = x.orElse(y)
    }

    def updatedL(lens: T => T): ActionResult[M] =
      updated(lens(value))
  }
}

package object handlers {
  val VoidEffect: Effect = Effect(Future(NoAction: Action))
}
