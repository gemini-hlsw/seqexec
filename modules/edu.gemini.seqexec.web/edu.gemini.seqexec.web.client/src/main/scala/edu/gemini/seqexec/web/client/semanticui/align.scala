package edu.gemini.seqexec.web.client.semanticui

import scalaz.Equal

sealed trait Aligned

object Aligned {
  case object None extends Aligned
  case object Left extends Aligned
  case object Center extends Aligned
  case object Right extends Aligned

  implicit val equal: Equal[Aligned] = Equal.equalA[Aligned]
}

trait SemanticUIAlign extends Aligned
