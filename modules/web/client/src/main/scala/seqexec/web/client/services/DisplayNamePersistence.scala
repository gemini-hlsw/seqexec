// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.services

import cats.syntax.all._
import org.scalajs.dom.window
import scala.concurrent.Future
import scala.concurrent.ExecutionContext

trait DisplayNamePersistence {
  def storedDisplayNames: Map[String, String] = {
    import io.circe.parser.decode
    (for {
      ls <- Option(window.localStorage)
      dn <- Option(ls.getItem("displayNames"))
      m  <- decode[Map[String, String]](dn).toOption
    } yield m).getOrElse(Map.empty)
  }

  def persistDisplayName(result: Map[String, String])(implicit ec: ExecutionContext): Future[Unit] =
    Future {
      import io.circe.syntax._
      (for {
        ls <- Option(window.localStorage)
        m  <- Option(result.asJson.spaces2SortKeys)
      } yield ls.setItem("displayNames", m))
    }.void

  def removeDisplayName(result: Map[String, String])(implicit ec: ExecutionContext): Future[Unit] =
    Future {
      import io.circe.syntax._
      (for {
        ls <- Option(window.localStorage)
        m  <- Option(result.asJson.spaces2SortKeys)
      } yield ls.setItem("displayNames", m))
    }.void

}
