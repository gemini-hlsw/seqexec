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

}
