package edu.gemini.seqexec.web.common

object Dummy {

  object Sequences {
    val seq1: Sequence = Sequence(
      id = "First",
      state = SequenceState.NotRunning,
      instrument = "GMOS-S",
      steps = SequenceSteps(
        List(
          Step(
            id = 1,
            state = StepState.NotDone,
            config = Nil,
            file = None
          ),
          Step(
            id = 1,
            state = StepState.NotDone,
            config = Nil,
            file = None
          )
        )
      ),
      error = None
    )

    val seq2: Sequence = Sequence(
      id = "Second",
      state = SequenceState.NotRunning,
      instrument = "GMOS-S",
      steps = SequenceSteps(
        List(
          Step(
            id = 1,
            state = StepState.NotDone,
            config = Nil,
            file = None
          ),
          Step(
            id = 1,
            state = StepState.NotDone,
            config = Nil,
            file = None
          )
        )
      ),
      error = None
    )

    val seq1exe1: Sequence = Sequence(
      id = "First",
      state = SequenceState.Running,
      instrument = "GMOS-S",
      steps = SequenceSteps(
        List(
          Step(
            id = 1,
            state = StepState.Running,
            config = Nil,
            file = None
          ),
          Step(
            id = 1,
            state = StepState.NotDone,
            config = Nil,
            file = None
          )
        )
      ),
      error = None
    )

    val seq1exe2: Sequence = Sequence(
      id = "First",
      state = SequenceState.Running,
      instrument = "GMOS-S",
      steps = SequenceSteps(
        List(
          Step(
            id = 1,
            state = StepState.Done,
            config = Nil,
            file = Some("step1.fits")
          ),
          Step(
            id = 1,
            state = StepState.Running,
            config = Nil,
            file = None
          )
        )
      ),
      error = None
    )
  }

  object Queue {
    import Dummy.Sequences._
    val q0 = SeqexecQueue(List(seq1, seq2))
    val q1 = SeqexecQueue(List(seq1exe1, seq2))
    val q2 = SeqexecQueue(List(seq1exe2, seq2))
  }
}
