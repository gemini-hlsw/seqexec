package gem
package telnetd

import tuco._, Tuco._
import scalaz.effect._

object Main extends SafeApp {

  override def runc: IO[Unit] =
    Config(Interaction.interact, 6666).run(simpleServer)

}
