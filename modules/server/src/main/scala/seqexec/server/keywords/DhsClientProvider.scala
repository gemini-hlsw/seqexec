// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.server.keywords

trait DhsClientProvider[F[_]] {
  def dhsClient(instrumentName: String): DhsClient[F]
}
