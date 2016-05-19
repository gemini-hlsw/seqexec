package edu.gemini.seqexec.web.server.security

import com.unboundid.ldap.sdk._
import com.unboundid.util.Debug

import scala.collection.JavaConverters._
import scalaz._
import Scalaz._

sealed trait AuthenticationFailure
case class UserNotFound(user: String) extends AuthenticationFailure
case class BadCredentials(user: String) extends AuthenticationFailure
case class GenericFailure(msg: String) extends AuthenticationFailure

case class UserDetails(username: String, displayName: String)

/**
  * Handles the connections to the LDAP server
  */
class LDAPService(host: String, port: Int) {
  val MaxConnections = 20
  val Domain = "@gemini.edu"
  val UidExtractor = s"(\\w*)($Domain)?".r

  val connection = new LDAPConnection(host, port)
  val pool = new LDAPConnectionPool(connection, MaxConnections)
  //Debug.setEnabled(true)

  def authenticateUser(username: String, password: String): AuthenticationFailure \/ UserDetails = {
    val c = pool.getConnection
    try {
      // Let users enter with or without the domain
      val usernameWithDomain = if (username.endsWith(Domain)) username else s"$username$Domain"
      // Uid shouldn't have domain
      val uid = usernameWithDomain match {
        case UidExtractor(u, _) => u
        case _                  => username
      }
      val bindRequest = new SimpleBindRequest(usernameWithDomain, password)
      // Authenticate
      c.bind(bindRequest)

      // Find user details
      val baseDN = c.getRootDSE.getAttributeValue("namingContexts")
      val filter = Filter.createANDFilter(
        Filter.createEqualityFilter("uid", uid),
        Filter.createEqualityFilter("objectClass", "user")
      )
      val search = new SearchRequest(s"cn=users,$baseDN", SearchScope.SUB, filter)
      val searchResult = c.search(search)

      val displayName = for {
          s <- searchResult.getSearchEntries.asScala.headOption
          a <- Option(s.getAttribute("displayName"))
          d <- Option(a.getValue)
        } yield d

      UserDetails(username, ~displayName).right
    } catch {
      case e:LDAPException if e.getResultCode == ResultCode.NO_SUCH_OBJECT      =>
        e.printStackTrace()
        BadCredentials(username).left
      case e:LDAPException if e.getResultCode == ResultCode.INVALID_CREDENTIALS =>
        UserNotFound(username).left
      case e:LDAPException =>
        e.printStackTrace()
        GenericFailure("LDAP Authentication error").left
      case e:Exception =>
        e.printStackTrace()
        GenericFailure(e.getMessage).left
    } finally {
      pool.releaseConnection(c)
    }
  }
}
