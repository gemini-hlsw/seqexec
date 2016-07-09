package edu.gemini.seqexec.model

// Shared classes used for authentication
case class UserLoginRequest(username: String, password: String)
case class UserDetails(username: String, displayName: String)

object UserDetails {
  // Some useful type aliases for user elements
  type UID = String
  type DisplayName = String
  type Groups = List[String]
  type Thumbnail = Array[Byte]
}