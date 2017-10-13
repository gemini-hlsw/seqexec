// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package edu.gemini.seqexec.model

import edu.gemini.seqexec.model.Model.Resource

/**
  * Created by jluhrs on 10/13/17.
  */
sealed trait ActionType

object ActionType {
  object Observe extends ActionType
  // Used in tests
  object Undefined extends ActionType
  final case class Configure(sys: Resource) extends ActionType
}
