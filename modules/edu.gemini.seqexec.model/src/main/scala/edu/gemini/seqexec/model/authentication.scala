package edu.gemini.seqexec.model

// Shared classes used for authentication
case class UserLoginRequest(username: String, password: String)
case class UserDetails(username: String, displayName: String)
