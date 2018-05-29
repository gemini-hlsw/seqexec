// Copyright (c) 2016-2017 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import cats._
import cats.effect.IO
import cats.free.Free
import cats.implicits._
import com.unboundid.ldap.sdk._
import seqexec.model.UserDetails
import seqexec.web.server.security.AuthenticationService.AuthResult
import fs2.Stream
import org.log4s.getLogger

/**
  * Definition of LDAP as a free monad algebra/interpreters
  */
object FreeLDAPAuthenticationService {
  import LdapConnectionOps._
  import UserDetails._

  sealed trait LdapOp[A]
  // Operations on ldap
  object LdapOp {
    // Operation to authenticate a user, It returns true if it works
    final case class AuthenticateOp(username: String, password: String) extends LdapOp[UID]
    // Read the user display name
    final case class UserDisplayNameOp(uid: UID)                        extends LdapOp[DisplayName]
    // Reads the name, groups and thumbnail
    final case class DisplayNameGrpThumbOp(uid: UID)                    extends LdapOp[(DisplayName, Groups, Option[Thumbnail])]
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

  // Run on IO
  def runIO[A](a: LdapM[A], c: LDAPConnection): IO[A] =
    a.foldMap(toIO(c))

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
class FreeLDAPAuthenticationService(hosts: List[(String, Int)]) extends AuthService {
  import FreeLDAPAuthenticationService._
  private implicit val resultEqual: Eq[ResultCode] = Eq.fromUniversalEquals

  private val Log = getLogger

  // Shorten the default timeout
  private val Timeout = 1000
  private val Domain = "@gemini.edu"

  lazy val ldapOptions: LDAPConnectionOptions = {
    val opts = new LDAPConnectionOptions()
    opts.setConnectTimeoutMillis(Timeout)
    opts
  }

  // Will attempt several servers in case they fail
  lazy val failoverServerSet = new FailoverServerSet(hosts.map(_._1).toArray, hosts.map(_._2).toArray, ldapOptions)

  override def authenticateUser(username: String, password: String): IO[AuthResult] = {
    // We should always return the domain
    val usernameWithDomain = if (username.endsWith(Domain)) username else s"$username$Domain"

    Stream.bracket(IO(failoverServerSet.getConnection))(c => Stream.eval(runIO(authenticationAndName(usernameWithDomain, password), c)), c => IO(c.close())).compile.last.attempt.map {
      case Left(e: LDAPException) if e.getResultCode === ResultCode.NO_SUCH_OBJECT      =>
        Log.error(e)(s"Exception connection to LDAP server: ${e.getExceptionMessage}")
        BadCredentials(username).asLeft
      case Left(e: LDAPException) if e.getResultCode === ResultCode.INVALID_CREDENTIALS =>
        Log.error(e)(s"Exception connection to LDAP server: ${e.getExceptionMessage}")
        UserNotFound(username).asLeft
      case Left(e: LDAPException)                                                       =>
        Log.error(e)(s"Exception connection to LDAP server: ${e.getExceptionMessage}")
        GenericFailure("LDAP Authentication error").asLeft
      case Left(e: Throwable)                                                           =>
        GenericFailure(e.getMessage).asLeft
      case Right(u)                                                                     =>
        u.fold(UserNotFound(username).asLeft[UserDetails])(_.asRight)
    }
  }
}
