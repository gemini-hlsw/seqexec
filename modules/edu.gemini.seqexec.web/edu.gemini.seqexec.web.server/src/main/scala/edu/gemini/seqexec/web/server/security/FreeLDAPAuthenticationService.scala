package edu.gemini.seqexec.web.server.security

import com.unboundid.ldap.sdk._
import edu.gemini.seqexec.web.common.UserDetails
import edu.gemini.seqexec.web.server.security.AuthenticationService.AuthResult

import scalaz.Scalaz._
import scalaz._
import scalaz.concurrent.Task
import scalaz.effect.IO

/**
  * Definition of LDAP as a free monad algebra/interpreters
  */
object FreeLDAPAuthenticationService {
  import UserDetails._
  import LdapConnectionOps._

  sealed trait LdapOp[A]
  // Operations on ldap
  object LdapOp {
    // Operation to authenticate a user, It returns true if it works
    case class AuthenticateOp(username: String, password: String) extends LdapOp[UID]
    // Read the user display name
    case class UserDisplayNameOp(uid: UID)                        extends LdapOp[DisplayName]
    // Reads the name, groups and thumbnail
    case class DisplayNameGrpThumbOp(uid: UID)                    extends LdapOp[(DisplayName, Groups, Option[Thumbnail])]
  }

  // Free monad over the free functor of LdapOp.
  type LdapM[A] = Free[LdapOp, A]

  // Smart constructors for LdapOp[A]
  def bind(u: String, p: String): LdapM[UID]                                   = Free.liftF(LdapOp.AuthenticateOp(u, p))
  def displayName(u: UID): LdapM[DisplayName]                                  = Free.liftF(LdapOp.UserDisplayNameOp(u))
  def nameGroupsThumb(u: UID): LdapM[(DisplayName, Groups, Option[Thumbnail])] = Free.liftF(LdapOp.DisplayNameGrpThumbOp(u))

  // Natural transformation to IO
  def toIO(c: LDAPConnection): LdapOp ~> IO =
    new (LdapOp ~> IO) {
      def apply[A](fa: LdapOp[A]) =
        fa match {
          case LdapOp.AuthenticateOp(u, p)       => IO(c.authenticate(u, p))
          case LdapOp.UserDisplayNameOp(uid)     => IO(c.displayName(uid))
          case LdapOp.DisplayNameGrpThumbOp(uid) => IO(c.nameGroupsThumb(uid))
      }
    }

  // Natural transformation to Task
  def toTask(c: LDAPConnection): LdapOp ~> Task =
    new (LdapOp ~> Task) {
      def apply[A](fa: LdapOp[A]) =
        fa match {
          case LdapOp.AuthenticateOp(u, p)       => Task(c.authenticate(u, p))
          case LdapOp.UserDisplayNameOp(uid)     => Task(c.displayName(uid))
          case LdapOp.DisplayNameGrpThumbOp(uid) => Task(c.nameGroupsThumb(uid))
      }
    }

  // Run on IO
  def runIO[A](a: LdapM[A], c: LDAPConnection): IO[A] =
    a.foldMap(toIO(c))

  // Run on task, it could be used with http4s eventually
  def runTask[A](a: LdapM[A], c: LDAPConnection): Task[A] =
    a.foldMap(toTask(c))

  // Programs
  // Does simple user authentication
  def authenticate(u: String, p: String): LdapM[UID] = bind(u, p)

  // Authenticate and reads the display name
  def authenticationAndName(u: String, p: String): LdapM[UserDetails] = for {
    u <- bind(u, p)
    d <- displayName(u)
  } yield UserDetails(u, d)

  // Authenticate and reads the name, groups and photo
  def authNameGroupThumb(u: String, p: String): LdapM[(UserDetails, Groups, Option[Thumbnail])] = for {
    u <- bind(u, p)
    d <- nameGroupsThumb(u)
  } yield (UserDetails(u, d._1), d._2, d._3)
}

/**
  * Handles authentication against the AD/LDAP server
  */
class FreeLDAPAuthenticationService(host: String, port: Int) extends AuthenticationService {
  import FreeLDAPAuthenticationService._

  val MaxConnections = 20
  // Shorten the default timeout
  val Timeout = 1000
  val Domain = "@gemini.edu"

  lazy val connection = new LDAPConnection(new LDAPConnectionOptions() <| {_.setConnectTimeoutMillis(Timeout)}, host, port)
  lazy val pool = new LDAPConnectionPool(connection, MaxConnections)

  override def authenticateUser(username: String, password: String): AuthResult = {
    // We should always return the domain
    val usernameWithDomain = if (username.endsWith(Domain)) username else s"$username$Domain"

    // We may want to run this directly on Task
    \/.fromTryCatchNonFatal {
      val c = pool.getConnection
      runIO(authenticationAndName(usernameWithDomain, password), c)
        .ensuring(IO(pool.releaseConnection(c))).unsafePerformIO()
    }.leftMap {
      case e:LDAPException if e.getResultCode == ResultCode.NO_SUCH_OBJECT      =>
        BadCredentials(username)
      case e:LDAPException if e.getResultCode == ResultCode.INVALID_CREDENTIALS =>
        UserNotFound(username)
      case e:LDAPException =>
        GenericFailure("LDAP Authentication error")
      case e:Exception =>
        GenericFailure(e.getMessage)
    }
  }
}
