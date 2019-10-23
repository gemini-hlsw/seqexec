// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.model

import cats.Eq

// Shared classes used for authentication
final case class UserLoginRequest(username: String, password: String)

object UserLoginRequest {
  implicit val eq: Eq[UserLoginRequest] = Eq.fromUniversalEquals
}

final case class UserDetails(username: String, displayName: String)

object UserDetails {
  // Some useful type aliases for user elements
  type UID = String
  type DisplayName = String
  type Groups = List[String]
  type Thumbnail = Array[Byte]

  implicit val eq: Eq[UserDetails] = Eq.fromUniversalEquals
}
