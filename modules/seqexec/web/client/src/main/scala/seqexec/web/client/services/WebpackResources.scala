// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.client.services

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import scalaz.Show
import scalaz.syntax.show._

object WebpackResources {

  // marker trait
  trait WebpackResource extends js.Object

  object WebpackResource {
    implicit val show: Show[WebpackResource] = Show.showFromToString
  }

  implicit class WebpackResourceOps(val r: WebpackResource) extends AnyVal {
    def resource: String = r.shows
  }

  @JSImport("semantic-ui-less/semantic.less", JSImport.Default)
  @js.native
  object SemanticUILessResource extends WebpackResource

  @JSImport("less/style.less", JSImport.Default)
  @js.native
  object SeqexecLessResource extends WebpackResource

  @JSImport("sounds/sequencepaused.mp3", JSImport.Default)
  @js.native
  object SequencePausedResource extends WebpackResource

  @JSImport("sounds/exposurepaused.mp3", JSImport.Default)
  @js.native
  object ExposurePausedResource extends WebpackResource

  @JSImport("sounds/sequenceerror.mp3", JSImport.Default)
  @js.native
  object SequenceErrorResource extends WebpackResource

  @JSImport("sounds/sequencecomplete.mp3", JSImport.Default)
  @js.native
  object SequenceCompleteResource extends WebpackResource

  @JSImport("sounds/beep-22.mp3", JSImport.Default)
  @js.native
  object BeepResource extends WebpackResource

}
