package edu.gemini.seqexec.web.client.semanticui.elements.message

/**
  * Common code for all Message components
  */
trait Message {
  sealed trait Style

  object Style {
    case object NotDefined extends Style
    case object Warning extends Style
    case object Info extends Style
    case object Positive extends Style
    case object Success extends Style
    case object Negative extends Style
    case object Error extends Style
  }
}
