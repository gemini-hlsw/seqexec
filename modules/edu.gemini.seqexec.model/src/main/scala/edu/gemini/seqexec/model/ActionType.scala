// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import edu.gemini.seqexec.model.Model.Resource

import cats.Eq

/**
  * Created by jluhrs on 10/13/17.
  */
sealed trait ActionType extends Product with Serializable

object ActionType {

  case object Observe extends ActionType
  // Used in tests
  case object Undefined extends ActionType
  final case class Configure(sys: Resource) extends ActionType

  implicit val equal: Eq[ActionType] = Eq.fromUniversalEquals

}
