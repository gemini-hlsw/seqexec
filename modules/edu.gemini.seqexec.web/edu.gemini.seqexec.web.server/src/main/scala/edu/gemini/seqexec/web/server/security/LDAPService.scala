package edu.gemini.seqexec.web.server.security

import com.unboundid.ldap.sdk._
import edu.gemini.seqexec.web.common.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult

import scala.collection.JavaConverters._
import scalaz._
import Scalaz._

/**
  * Handles the connections to the LDAP server
  */
class LDAPService(host: String, port: Int) extends AuthenticationService {
  val MaxConnections = 20
  val Domain = "@gemini.edu"
  val UidExtractor = s"(\\w*)($Domain)?".r
  // Shorten the timeout
  val Timeout = 1000

  lazy val connection = new LDAPConnection(new LDAPConnectionOptions() <| {_.setConnectTimeoutMillis(Timeout)}, host, port)
  lazy val pool = new LDAPConnectionPool(connection, MaxConnections)

  override def authenticateUser(username: String, password: String): AuthResult = {
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
