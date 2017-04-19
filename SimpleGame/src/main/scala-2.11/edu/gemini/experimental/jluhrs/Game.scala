package edu.gemini.experimental.jluhrs

import EventStream._
import Timer._

import scalaz._
import Scalaz._
import scalaz.stream.Process
import scalaz.concurrent.Task
import scala.concurrent.duration._

/**
  * Created by jluhrs on 5/24/16.
  */

/* The game logic is here.
 * It's just an object that moves at constant speed, and refresh its position at given times.
 * The direction of movement is affected by some input events.
 */
object Game {

  case class Position(x: Double, y: Double)
  case class SpeedDir(x: Double, y: Double)

  sealed trait Input

  case object TimeTick extends Input

  case class GameState(t: Time, position: Position, speedDir: SpeedDir, velocity: Double, boardWidth: Int, boardHeight: Int, exit: Boolean)

  sealed trait PlayerInput extends Input

  case object Exit extends PlayerInput

  sealed trait DirectionalInput extends PlayerInput

  case object Up extends DirectionalInput
  case object Down extends DirectionalInput
  case object Right extends DirectionalInput
  case object Left extends DirectionalInput

  def toSpeedDir(i: DirectionalInput): SpeedDir = i match {
    case Up => SpeedDir(0.0, 1.0)
    case Down => SpeedDir(0.0, -1.0)
    case Right => SpeedDir(1.0, 0.0)
    case Left => SpeedDir(-1.0, 0.0)
  }

  def updateSpeed(in: DirectionalInput):State[GameState, Unit] = State[GameState, Unit] { s => (s.copy(speedDir = toSpeedDir(in)), ()) }

  def move(delta: Time, p0: Position, dir: SpeedDir, velocity: Double):Position =
    Position(p0.x + delta.toUnit(SECONDS) * velocity * dir.x, p0.y + delta.toUnit(SECONDS) * velocity * dir.y)

  def tick(time: Time):State[GameState, Unit] = {
    def mod(x:Double, b: Double) = if (x%b < 0) (b + x%b) else x%b
    def modPos(pos: Position, width: Double, height: Double) = Position(mod(pos.x, width.toDouble), mod(pos.y, height.toDouble))


    State[GameState, Unit] { s =>
      (s.copy(t=time, position=modPos(move(time-s.t, s.position, s.speedDir, s.velocity), s.boardWidth, s.boardHeight ) ), ())
    }
  }

  def exit: State[GameState, Unit] = State[GameState, Unit] {
    s => (s.copy(exit = true), ())
  }

  // State machine to run one game cycle.
  def gameCycle(in: Event[Input]): State[GameState, \/[GameState, GameState]] = in match {
    case (_, d: DirectionalInput) => (updateSpeed(d) *> get) map (_.left[GameState])
    case (_, Exit )               => (exit *> get) map (_.left[GameState])
    case (t: Time, TimeTick)      => (tick(t) *> get) map (_.right[GameState])
  }

  def runGame(width: Int, height: Int, input: Process[Task, Event[Input]]): Process[Task, Position] = {
    val speed = 5.0
    val startDir = SpeedDir(0.0, 1.0)
    val startPosition = Position(0.0, height/2.0)

    val s0 = GameState(0 seconds, startPosition, startDir, speed, width, height, false)

    input.stateScan(s0)(gameCycle).collect { case \/-(r) => r}.takeWhile(!_.exit).map(_.position)

  }

}
