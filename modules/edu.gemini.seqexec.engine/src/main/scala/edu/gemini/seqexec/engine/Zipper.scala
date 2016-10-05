package edu.gemini.seqexec.engine

import scalaz._
import Scalaz._

object Zipper {

  case class StepZ(
    pending: Step[Action],
    focus: Current,
    done: Step[Result]
  ) {

    val next: Option[StepZ] = for {
      exeDone <- focus.uncurrentify
      (exePending, step) <- pending.uncons
      curr <- Current.currentify(exePending)
    } yield StepZ(step, curr, done.cons(exeDone))


    val uncurrentify: Option[Step[Result]] =
      if (pending.isEmpty) focus.uncurrentify.map(done.cons)
      else None

  }

  object StepZ {

    def currentify(step0: Step[Action]): Option[StepZ] = for {
      (exe, step1) <- step0.uncons
      curr <- Current.currentify(exe)
      } yield StepZ(step1, curr, Step.empty(step0.id))

    val current: StepZ @> Current =
      Lens.lensu((s, f) => s.copy(focus = f), _.focus)

  }

  case class SequenceZ(
    pending: Sequence[Action],
    focus: StepZ,
    done: Sequence[Result]
  ) {

    val next: Option[SequenceZ] =
      focus.next match {
        // Step completed
        case None => for {
          stepDone <- focus.uncurrentify
          (stepPending, seq) <- pending.uncons
          curr <- StepZ.currentify(stepPending)
        } yield SequenceZ(seq, curr, done.cons(stepDone))
        // Current step ongoing
        case Some(stz) => Some(SequenceZ(pending, stz, done))
      }

    val uncurrentify: Option[Sequence[Result]] =
      if (pending.isEmpty) focus.uncurrentify.map(done.cons)
      else None
  }

  object SequenceZ {

    def currentify(seq0: Sequence[Action]): Option[SequenceZ] = for {
      (step, seq1) <- seq0.uncons
      curr <- StepZ.currentify(step)
    } yield SequenceZ(seq1, curr, Sequence.empty(seq0.id))

    private val focus: SequenceZ @> StepZ =
      Lens.lensu((s, f) => s.copy(focus = f), _.focus)

    val current: SequenceZ @> Current = focus >=> StepZ.current

  }

  case class QueueZ(
    pending: Queue[Action],
    focus: SequenceZ,
    done: Queue[Result]
  ) {

    val next: Option[QueueZ] =
      focus.next match {
        // Sequence completed
        case None =>
          for {
            seqDone <- focus.uncurrentify
            (seqPending, q) <- pending.uncons
            curr <- SequenceZ.currentify(seqPending)
          } yield QueueZ(q, curr, done.cons(seqDone))
          // XXX: This stacksoverflow
          // focus.uncurrentify.flatMap(
          //   seqDone => pending.uncons match {
          //     case None =>
          //       Some(QueueZ(Queue(Nil), focus, done.cons(seqDone)))
          //     case Some((seqPending, q)) =>
          //       SequenceZ.currentify(seqPending).map(
          //         curr => QueueZ(q, curr, done.cons(seqDone))
          //       )
          //   }
          // )
        // Current Sequence ongoing
        case Some(seqz) => Some(QueueZ(pending, seqz, done))
      }

    val uncurrentify: Option[Queue[Result]] =
      if (pending.isEmpty) focus.uncurrentify.map(done.cons)
      else None

  }

  object QueueZ {

    def currentify(queue0: Queue[Action]): Option[QueueZ] = for {
        (seq, queue1) <- queue0.uncons
        curr <- SequenceZ.currentify(seq)
      } yield QueueZ(queue1, curr, Queue(Nil))

    private val focus: QueueZ @> SequenceZ =
      Lens.lensu((q, f) => q.copy(focus = f), _.focus)

    val current: QueueZ @> Current = focus >=> SequenceZ.current

  }

}
