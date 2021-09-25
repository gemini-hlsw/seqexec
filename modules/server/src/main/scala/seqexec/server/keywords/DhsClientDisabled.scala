// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

import java.time.LocalDate
import java.time.format.DateTimeFormatter

import cats.effect.Sync
import cats.implicits._
import org.typelevel.log4cats.Logger
import seqexec.model.dhs.ImageFileId
import seqexec.model.dhs.toImageFileId
import seqexec.server.overrideLogMessage

class DhsClientDisabled[F[_]: Sync: Logger] extends DhsClient[F] {

  val format: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyyMMdd")

  override def createImage(p: DhsClient.ImageParameters): F[ImageFileId]                       = for {
    _    <- overrideLogMessage("DHS", "setKeywords")
    date <- Sync[F].delay(LocalDate.now)
    time <- Sync[F].delay(System.currentTimeMillis % 1000)
  } yield toImageFileId(f"S${date.format(format)}S$time%04d")

  override def setKeywords(id: ImageFileId, keywords: KeywordBag, finalFlag: Boolean): F[Unit] =
    overrideLogMessage("DHS", "setKeywords")
}
