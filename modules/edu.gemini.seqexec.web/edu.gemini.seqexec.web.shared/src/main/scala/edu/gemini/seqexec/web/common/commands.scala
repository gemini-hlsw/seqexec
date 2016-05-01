package edu.gemini.seqexec.web.common

// Classes exposed to web clients, uses only scala classes
case class RegularCommand(command: String, error: Boolean, response: String)
case class SequenceConfig(command: String, error: Boolean, response: String, keys: List[StepConfig])
case class SequenceStatus(command: String, error: Boolean, response: String, steps: List[String])
