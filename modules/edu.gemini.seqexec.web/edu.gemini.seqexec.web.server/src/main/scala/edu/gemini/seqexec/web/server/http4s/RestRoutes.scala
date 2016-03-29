package edu.gemini.seqexec.web.server.http4s

import org.http4s._
import org.http4s.dsl._
import upickle.default._

import edu.gemini.seqexec.web.common._

object RestRoutes {

  implicit val commentDecoder:EntityDecoder[Comment] = EntityDecoder.decodeBy(MediaRange.`text/*`)(msg =>
        collectBinary(msg).map(bs => read[Comment](new String(bs.toArray, msg.charset.getOrElse(Charset.`UTF-8`).nioCharset))
      )
  )

  val service = HttpService {
    case req @ GET -> Root / "api" / "seqexec" / "current" / "queue" =>
      Ok(write(currentQueue))
  }
}
