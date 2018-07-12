// Copyright (c) 2016-2018 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import cats.effect.IO
import seqexec.model.dhs.ImageFileId
import seqexec.server.SeqAction

/**
  * Typeclass for types that can send keywords
  */
trait HeaderProvider[A] {
  // Client to send keywords to an appropriate server
  def keywordsClient(a: A): KeywordsClient[IO]
}

object HeaderProvider {
  def apply[A](implicit ev: HeaderProvider[A]): HeaderProvider[A] = ev

  final class HeaderProviderOps[A: HeaderProvider](val a: A) {
    def keywordsClient: KeywordsClient[IO] = HeaderProvider[A].keywordsClient(a)
  }

  implicit def ToHeaderProviderOps[A: HeaderProvider](a: A): HeaderProviderOps[A] =
    new HeaderProviderOps(a)
}

/**
  * Header implementations know what headers sent before and after an observation
  */
trait Header {
  def sendBefore(id: ImageFileId): SeqAction[Unit]
  def sendAfter(id: ImageFileId): SeqAction[Unit]
}
