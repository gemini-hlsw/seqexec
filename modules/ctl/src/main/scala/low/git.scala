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

package gem.ctl
package low

import scalaz._, Scalaz._
import scala.util.matching.Regex

import gem.ctl.free.ctl._
import io._

/** Low-level constructors for `CtlIO` operations related to git. */
object git {

  case class Commit(hash: String)
  object Commit {
    implicit val OrderCommit: Order[Commit] = Order.orderBy(_.hash)
  }

  case class Tag(tag: String)

  // Yields most recent tag matching the given pattern, if any
  def mostRecentTag(pattern: Regex): CtlIO[Option[Tag]] =
    shell("git", "tag").require {
      case Output(0, ts) => ts.find(s => pattern.findFirstIn(s).isDefined).map(Tag)
    }

  // Yields the commit associated with the given revision, or fails with a nice message
  def commitForRevision(rev: String): CtlIO[Commit] =
    shell("git", "rev-parse", rev).require {
      case Output(0, List(s)) => Some(Commit(s))
      case Output(128, _)     => None
    } .flatMap {
      case Some(c) => c.point[CtlIO]
      case None    => error(s"No such revsion: $rev") *> exit[Commit](128)
    }

  val uncommittedChanges: CtlIO[Boolean] =
    shell("git status -s").require {
      case Output(0, deltas) => deltas.nonEmpty
    }

  // is c1 an ancestor of c2?
  def isAncestor(c1: Commit, c2: Commit): CtlIO[Boolean] =
    shell("git", "merge-base", "--is-ancestor", c1.hash, c2.hash).require {
      case Output(0, Nil) => true
      case Output(1, Nil) => false
    }

  def fileContentsAtCommit(c: Commit, path: String): CtlIO[List[String]] =
    shell("git", "show", s"${c.hash}:$path").require {
      case Output(0, ss) => ss
    }

  def fileContents(path: String): CtlIO[List[String]] =
    shell("cat", path).require {
      case Output(0, ss) => ss
    }

}
