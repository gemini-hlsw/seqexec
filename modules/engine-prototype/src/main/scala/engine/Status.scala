package engine

sealed trait Status
object Status {
  case object Running extends Status
  case object Waiting extends Status
  case object Finished extends Status
  case object Failed extends Status


//   whenRunning(sig)(f: ) = sig.get.flatMap(st =>
//     sig.get.flatMap(st =>
//       st.status match {
//         case Status.Running => fa
//         case _ => F.pure(Unit)
//       }
// )

}
