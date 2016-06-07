package edu.gemini.experimental.jluhrs

import EventStream.Event
import Timer._
import Game._
import CharDisplay._

/**
  * Created by jluhrs on 5/25/16.
  */
object Main {

  val width = 40
  val height = 20
  val border = 1


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

    //This is just to animate the frames. The real timing must come from the input timing stream.
    Thread.sleep(500)
  }

  def main(args: Array[String]): Unit =
    (runGame(width, height, periodicTimer(0.0, 1.0),
      Stream[Option[Event[PlayerInput]]](Some(5.0, Right), Some(15.0, Up), Some(25.0, Right))
    ).take(50).toList map (frame(width, height, border, _))).map(displayFrame)

}
