package engine

import cats.effect.Effect

import fs2.async.mutable.Signal

object Execution {

  def configureTCS[F[_]](implicit F: Effect[F]): F[Either[Failure, OK]]  = ???

  def configureInst[F[_]](name: String)(implicit F: Effect[F]): F[Either[Failure, OK]]  = ???

  def observe[F[_]](implicit F: Effect[F]): F[Either[Failure, OK]] = ???

  type Failure = Unit

  type OK = Unit

}
