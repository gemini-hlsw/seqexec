package edu.gemini.seqexec.model

case class UserLoginRequest(username: String, password: String)
case class UserDetails(username: String, displayName: String)
