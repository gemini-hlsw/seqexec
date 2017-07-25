// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package gem
package web

import argonaut._, Argonaut._, ArgonautShapeless._

import gem.{ Service => GemService }
import gem.json._

import org.http4s._, Http4s._
import org.http4s.argonaut._
import org.http4s.dsl._
import scalaz.concurrent._
// import scalaz._, Scalaz._


object WebService {

  // Anything with an EncodeJson can be returned. It's not clear why we don't get this for free.
  implicit def automaticJsonEncoderOf[A: EncodeJson]: EntityEncoder[A] = jsonEncoderOf

  def apply(gs: GemService[Task]): HttpService =
    HttpService {
      case GET -> Root / "program" / "all" =>
        gs.queryProgramsByName("%", 100).flatMap((es: List[Program[Nothing]]) => Ok(es.map(e => e.id)))
    }

}
