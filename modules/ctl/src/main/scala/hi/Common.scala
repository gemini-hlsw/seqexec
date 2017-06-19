/*
 * Copyright (c) 2017, Association of Universities for Research in Astronomy, Inc. (AURA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * 3. Neither the name of the copyright holder nor the names of its contributors
 *    may be used to endorse or promote products derived from this software without
 *    specific prior written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
 * ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package gem.ctl.hi

import gem.ctl.free.ctl._
import gem.ctl.low.git._
import gem.ctl.low.docker._

import scalaz._, Scalaz._

/** Constructors for some common `CtlIO` operations that are shared by other commands. */
object common {

  def getUniqueRunningContainerWithLabel(label: String): CtlIO[Container] =
    findRunningContainersWithLabel(label) flatMap {
      case c :: Nil => c.point[CtlIO]
      case Nil      => error("No running container found.")       *> exit(-1)
      case _        => error("Multiple running container found.") *> exit(-1)
    }

  def getRunningGemContainer: CtlIO[Container] =
    gosub("Finding current running Gem container.") {
      getUniqueRunningContainerWithLabel("edu.gemini.gem")
    }

  def getRunningPostgresContainer: CtlIO[Container] =
    gosub("Finding current running Postgres container.") {
      getUniqueRunningContainerWithLabel("edu.gemini.db")
    }

  def getDeployCommitForContainer(k: Container): CtlIO[DeployCommit] =
    gosub(s"Getting commit for container ${k.hash}.") {
      for {
        s <- getLabelValue("edu.gemini.commit", k)
        _ <- info(s"Commit is $s")
      } yield {
        val Suffix = "-UNCOMMITTED"
        if (s.endsWith(Suffix)) DeployCommit(Commit(s.dropRight(Suffix.length)), true)
        else                    DeployCommit(Commit(s),                          false)
      }
    }

  case class DeployCommit(commit: Commit, uncommitted: Boolean) {
    def imageVersion: String = if (uncommitted) s"${commit.hash}-UNCOMMITTED" else commit.hash
  }

}
