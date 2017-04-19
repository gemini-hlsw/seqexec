package edu.gemini.experimental.jluhrs

import Game._
import CharDisplay._

import scala.concurrent.duration._
import scalaz.stream.time._
import java.util.concurrent.Executors

/**
  * Created by jluhrs on 5/25/16.
  */
object Main {

  val width = 40
  val height = 20
  val border = 1

  implicit val scheduledExecutorService = Executors.newScheduledThreadPool(4)

  // Crude way to display the game board
  def displayFrame(s: String) = {
    //I couldn't find a sure way to clear the terminal, so this has to do.
    def clearScreen = {
      System.out.print("\n" * (height+2*border))
      System.out.flush();
    }
    clearScreen
    System.out.print(s)
    System.out.flush();
  }

  def main(args: Array[String]): Unit = {

    ConsoleKeys.start

    (runGame(width, height, (awakeEvery(100 milliseconds).map((_, TimeTick)) merge ConsoleKeys.inputP))
      map (frame(width, height, border, _))).map(displayFrame).run.unsafePerformSync

    ConsoleKeys.stop

    scheduledExecutorService.shutdown()
  }

}
