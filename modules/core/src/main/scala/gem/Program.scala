// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem

import scalaz._, Scalaz._

case class Program[A](id: Program.Id, title: String, observations: List[A])

object Program {

  type Id = edu.gemini.spModel.core.ProgramId
  val  Id = edu.gemini.spModel.core.ProgramId

  implicit val ProgramTraverse: Traverse[Program] =
    new Traverse[Program] {
      def traverseImpl[G[_]: Applicative, A, B](fa: Program[A])(f: A => G[B]): G[Program[B]] =
        fa.observations.traverse(f).map(os => fa.copy(observations = os))
    }

  implicit val OrderingProgramId: scala.math.Ordering[Id] =
    new scala.math.Ordering[Id] {
      def compare(x: Id, y: Id): Int = {
        (x.spOption, y.spOption) match {
          case (Some(xsp), Some(ysp)) => xsp.compareTo(ysp)
          case (_,         _        ) => x.toString.compareTo(y.toString)
        }
      }
    }

  implicit val OrderProgramId: Order[Id] =
    Order.fromScalaOrdering[Id]

}
