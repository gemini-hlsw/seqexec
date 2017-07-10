package engine

import scala.concurrent.ExecutionContext

import cats.{FlatMap, Functor}
import cats.data.NonEmptyList
import cats.effect.Effect
import cats.effect.implicits._
import cats.implicits._

import fs2.Stream
import fs2.async
import fs2.async.mutable.Signal

import monocle.Lens
import monocle.macros.GenLens

sealed trait Sequence {
  def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Unit]
}
object Sequence {

  sealed trait F2 extends Sequence
  object F2 {

    final case class Ready(
      done: List[Step.F2.Done],
      pending: List[Step.F2.Pending],
      current: Step.F2.Current
    ) extends F2 {
      def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Unit] = current.execute(m)
    }

    final case class Done(done: NonEmptyList[Step.F2.Done]) extends F2 {
      def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Unit] = F.pure(Unit)
    }

  }

  // final case class GMOS()

  final case class State(sequence: Sequence, status: Status) {

    def execute[F[_]](m: Sequence.State.Mutable[F])(implicit F: Effect[F], ec: ExecutionContext): F[Unit] = sequence.execute(m)
  }

  object State {

    val sequence: Lens[State, Sequence] = GenLens[State](_.sequence)

    val status: Lens[State, Status] = GenLens[State](_.status)

    // TODO: Make this an affine traversal or a Prism
    val current: Lens[State, Option[Either[Step.F2.Failed, Step.F2.Ongoing]]] = ???

    // Thread safe mutable but careful need to be careful not to block for very long.
    case class Mutable[F[_]](signal: Signal[F, Sequence.State]) {

      def getStatus(implicit F: Functor[F]): F[Status] = signal.get.map(_.status)

      def setStatus(s: Status)(implicit F: Functor[F]): F[Sequence.State] =
        signal.modify(State.status.set(s)).map(_.now)


      def getSequence(implicit F: Functor[F]): F[Sequence] = signal.get.map(_.sequence)

      def setSequence(seq: Sequence)(implicit F: Functor[F]): F[Sequence.State] =
        signal.modify(State.sequence.set(seq)).map(_.now)


      val getState: F[Sequence.State] = signal.get

      def setState(st: Sequence.State): F[Unit] = signal.set(st)


      // is it worth making Sequence.State.Monad a righteous Monad?
      def withState(f: Sequence.State => F[Sequence.State])(implicit F: FlatMap[F]): F[Sequence.State] =
        signal.get.flatMap(f)


      def modify(f: Sequence.State => Sequence.State)(implicit F: Functor[F]): F[Sequence.State] =
        signal.modify(f).map(_.now)

    }

    object Mutable {

      def stream[F[_]](st0: Sequence.State)(handle: (Mutable[F]) => F[Unit])(implicit F: Effect[F], ec: ExecutionContext): Stream[F, Sequence.State] =
        Stream.force(
          async.signalOf[F, Sequence.State](st0).flatMap(sig =>
            async.fork(handle(Mutable(sig))) *> sig.discrete.pure[F]
          )
        )

    }

  }

}
