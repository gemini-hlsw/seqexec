package engine

import scala.concurrent.ExecutionContext

import cats.effect.Effect
import cats.implicits._

import fs2.async.mutable.Signal

import monocle.Lens
import monocle.macros.GenLens

sealed trait Sequence {
  def execute[F[_]](sig: Signal[F, Sequence.State])(implicit F: Effect[F], ec: ExecutionContext): F[Unit]
}

object Sequence {

  final case class F2(
    done: List[Step.F2.Done],
    pending: List[Step.F2.Pending],
    // Better?: Either[Step.F2.Failed, Option[Step.F2.Ongoing]]
    // None: No ongoing step
    // Some/Left: current step failed
    // Some/Right: current steop ongoing
    current: Option[Either[Step.F2.Failed, Step.F2.Ongoing]]
  ) extends Sequence {

    def execute[F[_]](sig: Signal[F, Sequence.State])(implicit F: Effect[F], ec: ExecutionContext): F[Unit] =
      current match {
        // No current Step
        case None => pending match {
          // No pending steps, done
          case Nil => sig.modify(State.status.set(Status.Finished)).void
          // More pending steps
          case (next :: remainder) => next.execute(sig).flatMap {
            case Left(e) => sig.set(
              Sequence.State(
                F2(done, remainder, Some(Left(e))),
                Status.Failed
              )
            )
            case Right(d) =>
              sig.modify(
                State.sequence.set(
                  F2((d :: done), remainder, None)
                )
              ).flatMap(_.now.sequence.execute(sig)) // XXX: Check status running before executing
          }
        }
        case Some(_) => F.pure(Unit) // Event: Tried execute Sequence with an already ongoing step.
      }
  }

  final case class GMOS()


  final case class State(sequence: Sequence, status: Status)


  object State {

    val sequence: Lens[State, Sequence] = GenLens[State](_.sequence)

    val status: Lens[State, Status] = GenLens[State](_.status)

    // TODO: Make this an affine traversal or a Prism
    val current: Lens[State, Option[Either[Step.F2.Failed, Step.F2.Ongoing]]] = ???

  }

}
