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

import scalaz._, Scalaz._

case class Observation[S, D](
  id: Observation.Id,
  title: String,
  staticConfig: S,
  steps: List[D])

object Observation {

  case class Id(pid: Program.Id, index: Int) {
    override def toString = s"$pid-$index"
  }
  object Id {

    def fromString(s: String): Option[Observation.Id] =
      s.lastIndexOf('-') match {
        case -1 => None
        case  n =>
          val (a, b) = s.splitAt(n)
          b.drop(1).parseInt.toOption.map { n =>
            Observation.Id(Program.Id.parse(a), n)
          }
      }

    def unsafeFromString(s: String): Observation.Id =
      fromString(s).getOrElse(sys.error("Malformed Observation.Id: " + s))

    implicit val OrderingId: scala.math.Ordering[Id] =
      new scala.math.Ordering[Id] {
        def compare(x: Id, y: Id): Int =
          Program.OrderingProgramId.compare(x.pid, y.pid) match {
            case 0 => x.index compareTo y.index
            case i => i
          }
      }

    implicit val OrderId: Order[Id] =
      Order.fromScalaOrdering[Id]
  }

  implicit val ObservationBitraverse: Bitraverse[Observation] =
    new Bitraverse[Observation] {
      def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: Observation[A,B])(f: A => G[C], g: B => G[D]): G[Observation[C,D]] =
        (f(fab.staticConfig) |@| fab.steps.traverse(g))((c, d) => fab.copy(staticConfig = c, steps = d))
    }

}
