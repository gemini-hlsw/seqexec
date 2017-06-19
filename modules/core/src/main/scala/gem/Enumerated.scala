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

package gem

import scalaz._
import Scalaz._

/**
 * Typeclass for an enumerated type with unique string tags.
 * unsafeFromTag . tag = id
 */
trait Enumerated[A] extends Order[A] {
  def all: List[A]
  def tag(a: A): String
  def fromTag(s: String): Option[A] = all.find(tag(_) == s)
  def unsafeFromTag(tag: String): A = fromTag(tag).getOrElse(sys.error("Invalid tag: " + tag))
  def order(a: A, b: A): Ordering = Order[String].order(tag(a), tag(b))
}

object Enumerated {
  def apply[A](implicit ev: Enumerated[A]): Enumerated[A] = ev
}

trait Obsoletable[A] {
  def isActive(a: A): Boolean
  final def isObsolete(a: A): Boolean = !isActive(a)
}


/** Typeclass for things that can be show in a user interface. */
trait Display[A] {
  def name(a:A): String         // short name, for labels
  def elaboration(a:A): Option[String]  // an elaboration on the name, used for computing longname
  def longName(a: A): String = name(a) + elaboration(a).fold("")(n => s" ($n)")
}
