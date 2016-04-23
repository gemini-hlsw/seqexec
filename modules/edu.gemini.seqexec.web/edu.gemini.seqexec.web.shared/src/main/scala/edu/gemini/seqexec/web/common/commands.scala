package edu.gemini.seqexec.web.common

// Classes exposed to the clients converted to json
case class RegularCommand(command: String, error: Boolean, response: String)
case class SequenceConfig(command: String, error: Boolean, response: String, keys: List[StepConfig])
case class SequenceStatus(command: String, error: Boolean, response: String, steps: List[String])
