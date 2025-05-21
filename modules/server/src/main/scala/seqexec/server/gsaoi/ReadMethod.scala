// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.gsaoi

sealed trait ReadMethod extends Product with Serializable {
  val name: String
  val index: Int
}

object ReadMethod {
  case object Dcs extends ReadMethod {
    override val name: String = "DCS"
    override val index: Int   = 0
  }

  case object Fowler extends ReadMethod {
    override val name: String = "FOWLER"
    override val index: Int   = 1
  }

  case object Single extends ReadMethod {
    override val name: String = "SINGLE"
    override val index: Int   = 2
  }
}
