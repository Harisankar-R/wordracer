package com.codingnirvana.wordracer

import com.codingnirvana.wordracer.impl.MyWordRacer

import scala.io.StdIn

object Runner {

  def main(args: Array[String]) = {
    val stream = getClass.getResourceAsStream("/words.dat")
    val words = scala.io.Source.fromInputStream(stream).getLines.toIndexedSeq.sorted

    val wRacer = new MyWordRacer(words)


    wRacer.initGameBoard(StdIn.readChar())

    val myTurn = StdIn.readChar() == '1'

    (0 until 48).foldLeft(myTurn) { case (turn, i) =>
      if (turn) {
        val result = wRacer.pickLetter
        println(result.letter + " " + result.position)
        Console.flush()
      } else {
        val letter = StdIn.readChar()
        val position = wRacer.pickPosition(letter)
        println(position)
        Console.flush()
      }
      !turn
    }

    println(wRacer.initGameBoard('A'))
    System.exit(0)
  }

}
