package edu.gemini.experimental.jluhrs

import scala.math.max
import Game.Position

/**
  * Created by jluhrs on 5/25/16.
  */

// Creates a String representation of the game board.
object CharDisplay {

  def frame(width: Int, height: Int, border: Int, p: Position): String = {

    def addBorder(border: Int, s: String): String = {
      val lines = s.lines.toList
      lines.mkString("#"*(border*2+width) ++ "\n"++"#"*border,
        "#"*border++"\n"++"#"*border,
        "#"*border++"\n"++"#"*(border*2+width))
    }

    val x = Math.floor(p.x).toInt
    val y = height - Math.floor(p.y).toInt - 1

    addBorder(border,
      if(x>=0 && x<width && y>=0 && y<height)
        ((" " * width) ++ "\n") * y ++
          (" " * x ++ "O" ++ " " * (width - x - 1) ++
          "\n") ++ ((" " * width) ++ "\n") * (height - y - 1)
      else
        ((" " * width) ++ "\n") * height
    )
  }

}
