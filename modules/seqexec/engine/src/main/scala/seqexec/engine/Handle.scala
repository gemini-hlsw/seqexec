// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.engine

import cats.{Applicative, Monad}
import cats.data.StateT
import cats.effect.IO
import fs2.Stream

/**
  * Type constructor where all Seqexec side effect are managed.
  * Handle is a State machine inside a IO, which can produce Streams as output. It is combined with the
  * input stream to run seqexec engine.
  *
  * Its type parameters are:
  * A: Type of the output (usually Unit)
  * V: Type of the events
  * D: Type of the state machine state.
  */
final case class Handle[D, V, A](run: StateT[IO, D, (A, Option[Stream[IO, V]])])

object Handle {
  def fromStream[D, V](p: Stream[IO, V]): Handle[D, V, Unit] = {
    Handle[D, V, Unit](Applicative[StateT[IO, D, ?]].pure[(Unit, Option[Stream[IO, V]])](((), Some(p))))
  }

  implicit def handlePInstances[D, V]: Monad[Handle[D, V, ?]] = new Monad[Handle[D, V, ?]] {
    private def concatOpP[F[_]](op1: Option[Stream[F, V]],
                                op2: Option[Stream[F, V]]): Option[Stream[F, V]] = (op1, op2) match {
      case (None, None) => None
      case (Some(p1), None) => Some(p1)
      case (None, Some(p2)) => Some(p2)
      case (Some(p1), Some(p2)) => Some(p1 ++ p2)
    }

    override def pure[A](a: A): Handle[D, V, A] = Handle(Applicative[StateT[IO, D, ?]].pure((a, None)))

    override def flatMap[A, B](fa: Handle[D, V, A])(f: A => Handle[D, V, B]): Handle[D, V, B] = Handle[D, V, B](
      fa.run.flatMap {
        case (a, op1) => f(a).run.map {
          case (b, op2) => (b, concatOpP(op1, op2))
        }
      }
    )

    // Kudos to @tpolecat
    def tailRecM[A, B](a: A)(f: A => Handle[D, V, Either[A, B]]): Handle[D, V, B] = {
      // We don't really care what this type is
      type Unused = Option[Stream[IO, V]]

      // Construct a StateT that delegates to IO's tailRecM
      val st: StateT[IO, D, (B, Unused)] =
        StateT { s =>
          Monad[IO].tailRecM[(D, A), (D, (B, Unused))]((s, a)) {
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

  implicit class StateToHandle[D, V, A](self: StateT[IO, D, A]) {
    def toHandle: Handle[D, V, A] = Handle(self.map((_, None)))
  }

  def unit[D, V]: Handle[D, V, Unit] = Applicative[Handle[D, V, ?]].pure(())

  def get[D, V]: Handle[D, V, D] =
    StateT.get[IO, D].toHandle

  def inspect[D, V, A](f: D => A): Handle[D, V, A] =
    StateT.inspect[IO, D, A](f).toHandle

  def modify[D, V](f: D => D): Handle[D, V, Unit] =
    StateT.modify[IO, D](f).toHandle

}