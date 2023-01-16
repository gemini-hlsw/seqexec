// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.http4s

import java.time.Instant

import cats.effect.Sync
import cats.syntax.all._
import org.typelevel.log4cats.Logger
import org.http4s.headers.`User-Agent`
import seqexec.model.ClientId
import cats.effect.Ref

trait ClientsSetDb[F[_]] {
  def newClient(
    id:                ClientId,
    addr:              ClientsSetDb.ClientAddr,
    ua:                Option[ClientsSetDb.UserAgent]
  ): F[Unit]
  def removeClient(id: ClientId): F[Unit]
  def report: F[Unit]
}

object ClientsSetDb {
  type ClientAddr = String
  type UserAgent  = `User-Agent`
  type ClientsSet = Map[ClientId, (Instant, ClientAddr, Option[UserAgent])]

  def apply[F[_]: Sync: Logger](ref: Ref[F, ClientsSet]): ClientsSetDb[F] = new ClientsSetDb[F] {
    def newClient(id: ClientId, addr: ClientsSetDb.ClientAddr, ua: Option[UserAgent]): F[Unit] =
      Sync[F].delay(Instant.now).flatMap(i => ref.update(_ + (id -> ((i, addr, ua)))))
    def removeClient(id: ClientId): F[Unit]                                                    =
      ref.update(_ - id)
    def report: F[Unit]                                                                        =
      ref.get.flatMap { m =>
        Logger[F].debug("Clients Summary:") *>
          Logger[F].debug("----------------") *>
          m.map { case (id, (i, addr, ua)) =>
            s"  Client: $id, arrived on $i from addr: $addr ${ua.foldMap(u => s"UserAgent: $u")}"
          }.toList
            .traverse(Logger[F].debug(_)) *>
          Logger[F].debug("----------------")
      }

  }
}
