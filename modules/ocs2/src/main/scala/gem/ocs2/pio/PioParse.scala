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

package gem.ocs2.pio

import java.time.{Duration, Instant}

import scalaz._
import Scalaz._

final case class PioParse[A](run: String => Option[A]) {
  def apply(s: String): Option[A] = run(s)
}

object PioParse {
  implicit val FunctorPioParse = new Functor[PioParse] {
    def map[A, B](pa: PioParse[A])(f: A => B): PioParse[B] =
      PioParse(pa.run andThen (_.map(f)))
  }

  def enum[A](dictionary: (String, A)*): PioParse[A] =
    PioParse(dictionary.toMap.lift)

  // ********  Primitives

  val boolean: PioParse[Boolean] =
    PioParse(_.parseBoolean.toOption)

  val double: PioParse[Double] =
    PioParse(_.parseDouble.toOption)

  val int: PioParse[Int] =
    PioParse(_.parseInt.toOption)

  val long: PioParse[Long] =
    PioParse(_.parseLong.toOption)

  val string: PioParse[String] =
    PioParse(_.pure[Option])


  // ********  Java

  val instant: PioParse[Instant] =
    long.map(Instant.ofEpochMilli)

  val seconds: PioParse[Duration] =
    double.map { d => Duration.ofMillis((d * 1000).round) }
}
