package gem
package telnetd

import tuco.imports._
import gem.enum.ProgramRole

import scalaz._, Scalaz._
import scalaz.effect.IO

// session data
case class Data(
  service: Service[ConnectionIO]
)
object Data {
  object S { // service lenses
    def user[F[_]]: Service[F] @> User[ProgramRole] = Lens.lensu((a, b) => a.copy(user = b), _.user)
  }
  object L {
    val service: Data @> Service[ConnectionIO] = Lens.lensu((a, b) => a.copy(service = b), _.service)
    def user:   Data @> User[ProgramRole] = service >=> S.user
  }
}
