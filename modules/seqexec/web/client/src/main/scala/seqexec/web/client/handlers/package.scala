// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client

import cats.Monoid
import cats.implicits._
import diode.Action
import diode.ActionHandler
import diode.ActionResult
import diode.Effect
import diode.NoAction
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

package handlers {
  trait Handlers[M, T] { this: ActionHandler[M, T] =>
    implicit def pfMonoid[A, B]: Monoid[PartialFunction[A, B]] =
      new Monoid[PartialFunction[A, B]] {
        override def empty = PartialFunction.empty[A, B]
        override def combine(x: PartialFunction[A, B],
                             y: PartialFunction[A, B]): PartialFunction[A, B] =
          x.orElse(y)
      }

    def updatedL(lens: T => T): ActionResult[M] =
      updated(lens(value))

    def updatedSilentL(lens: T => T): ActionResult[M] =
      updatedSilent(lens(value))

    def updatedLE(lens: T => T, effect: Effect): ActionResult[M] =
      updated(lens(value), effect)

    def requestEffect[A, B <: Action, C <: Action](a: A,
                                                   f: A => Future[Unit],
                                                   m: A => B,
                                                   r: A => C): Effect =
      Effect(
        f(a)
          .as(m(a))
          .recover {
            case _ => r(a)
          }
      )

    def requestEffect2[A, B, C <: Action, D <: Action](
      a: (A, B),
      f: (A, B) => Future[Unit],
      m: A => C,
      r: A => D): Effect =
      Effect(
        f(a._1, a._2)
          .as(m(a._1))
          .recover {
            case _ => r(a._1)
          }
      )

  }
}

package object handlers {
  val VoidEffect: Effect = Effect(Future(NoAction: Action))
}
