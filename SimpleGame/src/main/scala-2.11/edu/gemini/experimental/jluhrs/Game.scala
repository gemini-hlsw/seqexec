package edu.gemini.experimental.jluhrs

import EventStream._
import Timer._
import scalaz._
import Scalaz._

/**
  * Created by jluhrs on 5/24/16.
  */

/* The game logic is here.
 * It's just an object that moves at constant speed, and refresh its position at given times.
 * The direction of movement is affected by some input events.
 */
object Game {

  type Position = (Double, Double)
  type SpeedDir = (Double, Double)

  case class GameState(t: Time, position: Position, speedDir: SpeedDir, velocity: Double, boardWidth: Int, boardHeight: Int)

  sealed trait PlayerInput

  sealed trait DirectionalInput extends PlayerInput

  case object Up extends DirectionalInput
  case object Down extends DirectionalInput
  case object Right extends DirectionalInput
  case object Left extends DirectionalInput

  implicit def decodeSpeedDir(i: DirectionalInput): SpeedDir = i match {
    case Up => (0.0, 1.0)
    case Down => (0.0, -1.0)
    case Right => (1.0, 0.0)
    case Left => (-1.0, 0.0)
  }

  def updateSpeed(in: Option[PlayerInput]):State[GameState, Unit] = State[GameState, Unit] { s => {
      in map {
        case v: DirectionalInput => (s.copy(speedDir = v), ())
        case _ => (s, ())
      } getOrElse ((s, ()))
    }
  }

  def move(delta: Time, p0: Position, dir: SpeedDir, velocity: Double):Position =
    (p0._1 + delta*velocity*dir._1, p0._2 + delta*velocity*dir._2)

  def tick(time: Time):State[GameState, Unit] = {
    def mod(x:Double, b: Double) = if (x%b < 0) (b + x%b) else x%b
    def modPos(pos: Position, width: Double, height: Double) = (mod(pos._1, width.toDouble), mod(pos._2, height.toDouble))

    State[GameState, Unit] { s => (s.copy(t=time, position=modPos(move(time-s.t, s.position, s.speedDir, s.velocity), s.boardWidth, s.boardHeight ) ), ())}
  }

  // State machine to run one game cycle.
  def gameCycle(in: Option[PlayerInput], time: Time): State[GameState, Unit] =
    for {
      _ <- updateSpeed(in)
      _ <- tick(time)
    } yield ()

  // Returns the most recent player input, for a given time
  def getUserInput(t: Time, l: => EventStream[PlayerInput]): (EventStream[PlayerInput], (Option[PlayerInput], Time)) = {
    val v = EventStream.pastEvents(l, t)
    (v._2, (v._1.reverse.headOption.map(_._2), t))
  }


  // Combines the time stream with the player input stream.
  def timeInputStream(timer: Timer, s: => EventStream[PlayerInput]): Stream[(Option[PlayerInput], Time)] = {
    val r = getUserInput(timer.head, s)
    r._2 #:: timeInputStream(timer.tail, r._1)
  }


  def runGameCycle(s0: GameState, ss: => Stream[State[GameState, Unit]]): Stream[Position] = {
    s0.position #:: runGameCycle(ss.head(s0)._1, ss.tail)
  }

  def runGame(width: Int, height: Int, timer: Timer, input: => EventStream[PlayerInput]): Stream[(Double, Double)] = {
    val speed = 1.0
    val startPosition = (0.0, 0.0)

    val machine = timeInputStream(timer.tail, input).map((i) => gameCycle(i._1, i._2))

    runGameCycle(GameState(timer.head, startPosition, (0.0, 1.0), speed, width, height), machine)

  }

}
