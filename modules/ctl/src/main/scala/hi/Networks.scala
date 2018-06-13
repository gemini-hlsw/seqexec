// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem.ctl.hi

import cats.implicits._
import gem.ctl.free.ctl._
import gem.ctl.low.docker._

object Networks {

  private val PrivateNetwork: String  = "gem-net"

  def getPrivateNetwork: CtlIO[Network] =
    gosub(s"Verifying $PrivateNetwork network.") {
      findNetwork(PrivateNetwork).flatMap {
        case Some(n) => info(s"Using existing network ${n.hash}.").as(n)
        case None    =>
          for {
            n <- createNetwork(PrivateNetwork)
            _ <- info(s"Created network ${n.hash}.")
          } yield n
      }
    }

}
