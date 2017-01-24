package edu.gemini.seqexec.web.client.semanticui

import scalaz._

sealed trait Size

object Size {
  case object NotSized extends Size
  case object Tiny extends Size
  case object Mini extends Size
  case object Medium extends Size
  case object Small extends Size
  case object Large extends Size
  case object Big extends Size
  case object Huge extends Size
  case object Massive extends Size
}

sealed trait Width

object Width {
  case object None extends Width
  case object One extends Width
  case object Two extends Width
  case object Three extends Width
  case object Four extends Width
  case object Five extends Width
  case object Six extends Width
  case object Seven extends Width
  case object Eight extends Width
  case object Nine extends Width
  case object Ten extends Width
  case object Eleven extends Width
  case object Twelve extends Width
  case object Thirteen extends Width
  case object Fourteen extends Width
  case object Fifteen extends Width
  case object Sixteen extends Width

  implicit val equal: Equal[Width] = Equal.equalA[Width]
}

sealed trait Aligned

object Aligned {
  case object None extends Aligned
  case object Left extends Aligned
  case object Center extends Aligned
  case object Right extends Aligned

  implicit val equal: Equal[Aligned] = Equal.equalA[Aligned]
}