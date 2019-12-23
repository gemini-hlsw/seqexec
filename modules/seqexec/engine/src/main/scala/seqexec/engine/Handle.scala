// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.{Applicative, Functor, Monad}
import cats.implicits._
import cats.data.StateT
import fs2.Stream

/**
  * Type constructor where all Seqexec side effect are managed.
  * Handle is a State machine inside a F, which can produce Streams as output. It is combined with the
  * input stream to run seqexec engine.
  *
  * Its type parameters are:
  * A: Type of the output (usually Unit)
  * V: Type of the events
  * D: Type of the state machine state.
  */
final case class Handle[F[_], D, V, A](run: StateT[F, D, (A, Option[Stream[F, V]])])

object Handle {
  def fromStream[F[_]: Monad, D, V](p: Stream[F, V]): Handle[F, D, V, Unit] = {
    Handle[F, D, V, Unit](Applicative[StateT[F, D, ?]].pure[(Unit, Option[Stream[F, V]])](((), Some(p))))
  }

  def liftF[F[_]: Monad, D, V, A](f: F[A]): Handle[F, D, V, A] = StateT.liftF[F, D, A](f).toHandle

  implicit def handlePMonad[F[_]: Monad, D, V]: Monad[Handle[F, D, V, ?]] = new Monad[Handle[F, D, V, ?]] {
    private def concatOpP(op1: Option[Stream[F, V]],
                          op2: Option[Stream[F, V]]): Option[Stream[F, V]] = (op1, op2) match {
      case (None, None) => None
      case (Some(p1), None) => Some(p1)
      case (None, Some(p2)) => Some(p2)
      case (Some(p1), Some(p2)) => Some(p1 ++ p2)
    }

    override def pure[A](a: A): Handle[F, D, V, A] = Handle(Applicative[StateT[F, D, ?]].pure((a, None)))

    override def flatMap[A, B](fa: Handle[F, D, V, A])(f: A => Handle[F, D, V, B]): Handle[F, D, V, B] = Handle[F, D, V, B](
      fa.run.flatMap {
        case (a, op1) => f(a).run.map {
          case (b, op2) => (b, concatOpP(op1, op2))
        }
      }
    )

    // Kudos to @tpolecat
    def tailRecM[A, B](a: A)(f: A => Handle[F, D, V, Either[A, B]]): Handle[F, D, V, B] = {
      // We don't really care what this type is
      type Unused = Option[Stream[F, V]]

      // Construct a StateT that delegates to F's tailRecM
      val st: StateT[F, D, (B, Unused)] =
        StateT { s =>
          Monad[F].tailRecM[(D, A), (D, (B, Unused))]((s, a)) {
            case (s, a) =>
              f(a).run.run(s).map {
                case (sʹ, (Left(a), _)) => Left((sʹ, a))
                case (sʹ, (Right(b), u)) => Right((sʹ, (b, u)))
              }
          }
        }

      // Done
      Handle(st)
    }
  }

  implicit class StateToHandle[F[_]: Functor, D, V, A](self: StateT[F, D, A]) {
    def toHandle: Handle[F, D, V, A] = Handle(self.map((_, None)))
  }

  def unit[F[_]: Monad, D, V]: Handle[F, D, V, Unit] =
    Applicative[Handle[F, D, V, ?]].unit

  def get[F[_]: Applicative, D, V]: Handle[F, D, V, D] =
    StateT.get[F, D].toHandle

  def inspect[F[_]: Applicative, D, V, A](f: D => A): Handle[F, D, V, A] =
    StateT.inspect[F, D, A](f).toHandle

  def modify[F[_]: Applicative, D, V](f: D => D): Handle[F, D, V, Unit] =
    StateT.modify[F, D](f).toHandle

}
