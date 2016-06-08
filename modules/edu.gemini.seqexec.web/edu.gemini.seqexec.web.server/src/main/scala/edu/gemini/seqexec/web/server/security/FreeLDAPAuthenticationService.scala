package edu.gemini.seqexec.web.server.security

import com.unboundid.ldap.sdk._
import edu.gemini.seqexec.web.common.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult

import scala.collection.JavaConverters._
import scala.util.Try
import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO

object FreeLDAPAuthenticationService {
  type UID = String
  type DisplayName = String

  val Domain = "@gemini.edu"
  val UidExtractor = s"(\\w*)($Domain)?".r

  implicit class LdapConnectionOps(val c: LDAPConnection) extends AnyVal {
    def authenticate(u: String, p: String): UID = {
      // It doesn't matter if we include the domain
      val usernameWithDomain = if (u.endsWith(Domain)) u else s"$u$Domain"
      val bindRequest = new SimpleBindRequest(usernameWithDomain, p)
      // Authenticate, it throws an exception if it fails
      c.bind(bindRequest)
      // Uid shouldn't have domain
      usernameWithDomain match {
        case UidExtractor(uid, _) => uid
        case uid                  => uid
      }
    }

    def displayName(uid: UID): DisplayName = {
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

      ~displayName
    }
  }

  sealed trait LdapOp[A]
  object LdapOp {
    // Operation to authenticate a user, It returs true if it works
    case class AuthenticateOp(username: String, password: String) extends LdapOp[UID]
    case class UserDisplayNameOp(uid: UID) extends LdapOp[DisplayName]
  }

  // Free monad over the free functor of LdapOp.
  type LdapM[A] = Free[LdapOp, A]

  // Smart constructors for LdapOp[A]
  def bind(u: String, p: String): LdapM[UID] = Free.liftF(LdapOp.AuthenticateOp(u, p))
  def displayName(u: UID): LdapM[DisplayName] = Free.liftF(LdapOp.UserDisplayNameOp(u))

  def authenticationProgram(u: String, p: String): LdapM[UserDetails] = for {
    u <- bind(u, p)
    d <- displayName(u)
  } yield UserDetails(u, d)

  // Function taking an Uboundid connection and producing A
  type LDAPConnectionReader[A] = LDAPConnection => A

  // Natural transformation to (LDAPConnectionReader[A])
  val toReader: LdapOp ~> LDAPConnectionReader =
    new (LdapOp ~> LDAPConnectionReader) {
      def apply[A](fa: LdapOp[A]) =
        fa match {
          case LdapOp.AuthenticateOp(u, p) => _.authenticate(u, p)
          case LdapOp.UserDisplayNameOp(uid: UID) => _.displayName(uid)
      }
    }

  // Natural transformation to IO
  def toIO(c: LDAPConnection): LdapOp ~> IO =
    new (LdapOp ~> IO) {
      def apply[A](fa: LdapOp[A]) =
        fa match {
          case LdapOp.AuthenticateOp(u, p) => IO(c.authenticate(u, p))
          case LdapOp.UserDisplayNameOp(uid: UID) => IO(c.displayName(uid))
      }
    }

  // Natural transformation to Task
  def toTask(c: LDAPConnection): LdapOp ~> Task =
    new (LdapOp ~> Task) {
      def apply[A](fa: LdapOp[A]) =
        fa match {
          case LdapOp.AuthenticateOp(u, p) => Task(c.authenticate(u, p))
          case LdapOp.UserDisplayNameOp(uid: UID) => Task(c.displayName(uid))
      }
    }

  def runIO[A](a: LdapM[A], c: LDAPConnection): IO[A] =
    a.foldMap(toIO(c))

  def runReader[A](a: LdapM[A]): LDAPConnectionReader[A] =
    a.foldMap(toReader)
}

/**
  * Handles authentication against the AD/LDAP server
  */
class FreeLDAPAuthenticationService(host: String, port: Int) extends AuthenticationService {
  import FreeLDAPAuthenticationService._

  val MaxConnections = 20
  // Shorten the default timeout
  val Timeout = 1000

  lazy val connection = new LDAPConnection(new LDAPConnectionOptions() <| {_.setConnectTimeoutMillis(Timeout)}, host, port)
  lazy val pool = new LDAPConnectionPool(connection, MaxConnections)

  override def authenticateUser(username: String, password: String): AuthResult = {
    val c = pool.getConnection
    try {
      \/-(runIO(authenticationProgram(username, password), c).unsafePerformIO())
    } catch {
      case e:LDAPException if e.getResultCode == ResultCode.NO_SUCH_OBJECT      =>
        BadCredentials(username).left
      case e:LDAPException if e.getResultCode == ResultCode.INVALID_CREDENTIALS =>
        UserNotFound(username).left
      case e:LDAPException =>
        GenericFailure("LDAP Authentication error").left
      case e:Exception =>
        GenericFailure(e.getMessage).left
    } finally {
      pool.releaseConnection(c)
    }
  }
}
