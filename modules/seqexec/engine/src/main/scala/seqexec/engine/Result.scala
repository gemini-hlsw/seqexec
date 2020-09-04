// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.Eq
import cats.syntax.all._

/**
  * The result of an `Action`.
  */
sealed trait Result[+F[_]] extends Product with Serializable {
  val errMsg: Option[String] = None
}

object Result {

  // Base traits for results. They make harder to pass the wrong value.
  trait RetVal
  trait PartialVal
  trait PauseContext[F[_]]

  final case class OK[R <: RetVal](response: R) extends Result[Nothing]
  final case class OKStopped[R <: RetVal](response: R) extends Result[Nothing]
  final case class Partial[R <: PartialVal](response: R) extends Result[Nothing]
  final case class Paused[F[_]](ctx: PauseContext[F]) extends Result[F]
  final case class OKAborted[R <: RetVal](response: R) extends Result[Nothing]
  // TODO: Replace the message by a richer Error type like `SeqexecFailure`
  final case class Error(msg: String) extends Result[Nothing] {
    override val errMsg: Option[String] = msg.some
  }
  object Error {
    implicit val eq: Eq[Error] = Eq.by(_.msg)
  }

}
