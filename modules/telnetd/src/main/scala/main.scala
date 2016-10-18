package gem
package telnetd

import gem.enum.ProgramRole

import tuco.imports._
import net.bmjames.opts._
import scalaz._, Scalaz._, scalaz.effect._

object Main extends SafeApp {

  val tmain: TelnetDIO[Unit] =
    for {
      _ <- FT.start // returns immediately
      _ <- FT.delay(System.out.println("Press <enter> to exit..."))
      _ <- FT.delay(System.in.read)
      _ <- FT.stop  // closes all connections
      _ <- FT.delay(System.out.println("Bye."))
    } yield ()

  override def runc: IO[Unit] = {
    class Example extends SafeShell(Interaction.interact) // required by internals of wrapped lib; will go away.
    val config = Config[Example](6666)
    config.run(tmain)
  }

}
