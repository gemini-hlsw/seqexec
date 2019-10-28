// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package seqexec.web.server.security

import cats._
import cats.effect._
import cats.free.Free
import cats.implicits._
import com.unboundid.ldap.sdk._
import io.chrisdavenport.log4cats.Logger
import seqexec.model.UserDetails
import seqexec.web.server.security.AuthenticationService.AuthResult

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
  def toF[F[_]: Sync](c: LDAPConnection): LdapOp ~> F =
    new (LdapOp ~> F) {
      def apply[A](fa: LdapOp[A]) =
        fa match {
          case LdapOp.AuthenticateOp(u, p)       => Sync[F].delay(c.authenticate(u, p))
          case LdapOp.UserDisplayNameOp(uid)     => Sync[F].delay(c.displayName(uid))
          case LdapOp.DisplayNameGrpThumbOp(uid) => Sync[F].delay(c.nameGroupsThumb(uid))
      }
    }

  // Run on IO
  def runF[F[_]: Sync, A](a: LdapM[A], c: LDAPConnection): F[A] =
    a.foldMap(toF(c))

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
class FreeLDAPAuthenticationService[F[_]: Sync: Logger](hosts: List[(String, Int)]) extends AuthService[F] {
  import FreeLDAPAuthenticationService._
  private implicit val resultEqual: Eq[ResultCode] = Eq.fromUniversalEquals

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

  override def authenticateUser(username: String, password: String): F[AuthResult] = {
    // We should always return the domain
    val usernameWithDomain = if (username.endsWith(Domain)) username else s"$username$Domain"

    val rsrc =
      for {
        c <- Resource.make(Sync[F].delay(failoverServerSet.getConnection))(c => Sync[F].delay(c.close()))
        x <- Resource.liftF(runF(authenticationAndName(usernameWithDomain, password), c).attempt)
      } yield x

    rsrc.use {
      case Left(e: LDAPException) if e.getResultCode === ResultCode.NO_SUCH_OBJECT      =>
        Logger[F].error(e)(s"Exception connection to LDAP server: ${e.getExceptionMessage}") *>
        BadCredentials(username).asLeft[UserDetails].pure[F].widen[AuthResult]
      case Left(e: LDAPException) if e.getResultCode === ResultCode.INVALID_CREDENTIALS =>
        Logger[F].error(e)(s"Exception connection to LDAP server: ${e.getExceptionMessage}") *>
        UserNotFound(username).asLeft[UserDetails].pure[F].widen[AuthResult]
      case Left(e: LDAPException)                                                       =>
        Logger[F].error(e)(s"Exception connection to LDAP server: ${e.getExceptionMessage}") *>
        GenericFailure("LDAP Authentication error").asLeft[UserDetails].pure[F].widen[AuthResult]
      case Left(e: Throwable)                                                           =>
        GenericFailure(e.getMessage).asLeft[UserDetails].pure[F].widen[AuthResult]
      case Right(u)                                                                     =>
        u.asRight.pure[F]
    }
  }

}
