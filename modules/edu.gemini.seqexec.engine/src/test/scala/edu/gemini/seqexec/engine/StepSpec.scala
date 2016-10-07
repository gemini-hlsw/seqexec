package edu.gemini.seqexec.engine

import org.scalatest.FlatSpec

/**
  * Created by jluhrs on 9/29/16.
  */
class StepSpec extends FlatSpec {

  // All tests check the output of running a step against the expected sequence of updates.

  // This test must have a simple step definition and the known sequence of updates that running that step creates.
  // The test will just run step and compare the output with the predefined sequence of updates.
  ignore should "run and generate the predicted sequence of updates." in {

  }

  // The difficult part is to set the pause command to interrupts the step execution in the middle.
  ignore should "stop execution in response to a pause command" in {

  }

  // It should reuse code from the previous test to set initial state.
  ignore should "resume execution from the non-running state in response to a resume command." in {

  }

  ignore should "ignore pause command if step is not been executed." in {

  }

  // Be careful that resume command really arrives while sequence is running.
  ignore should "ignore resume command if step is already running." in {

  }

  // For this test, one of the actions in the step must produce an error as result.
  ignore should "stop execution and propagate error when an Action ends in error." in {

  }


}
