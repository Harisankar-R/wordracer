package com.codingnirvana.wordracer

import com.codingnirvana.wordracer.impl.{Board, MyWordRacer}

import scala.util.Random

object LocalTester {

  def main(args: Array[String]) = {
    val time = System.currentTimeMillis()
    val startingLetter = ('A' + Random.nextInt(26)).toChar

    val stream = getClass.getResourceAsStream("/words.dat")
    val words = scala.io.Source.fromInputStream(stream).getLines.toIndexedSeq.sorted

    var firstBoard = Board(IndexedSeq.fill(7, 7)('*'), words)
    var secondBoard = Board(IndexedSeq.fill(7, 7)('*'), words)
    
    val player1 = new MyWordRacer(words)
    val player2 = new MyWordRacer(words)

    player1.initGameBoard(startingLetter)
    player2.initGameBoard(startingLetter)
    firstBoard = firstBoard.update(3, 3, startingLetter)
    secondBoard = secondBoard.update(3, 3, startingLetter)

    for (i <- 0 until 24) {
      val result1 = player1.pickLetter
      firstBoard = firstBoard.update(result1)

      val pos2 = player2.pickPosition(result1.letter)
      val result2 = Result(pos2, result1.letter)
      secondBoard = secondBoard.update(result2)

      val result3 = player2.pickLetter
      secondBoard = secondBoard.update(result3)

      val pos4 = player1.pickPosition(result3.letter)
      val result4 = Result(pos4, result3.letter)
      firstBoard = firstBoard.update(result4)
    }


    val score1 = firstBoard.calculateScore()
    val score2 = secondBoard.calculateScore()

    println("Player 1 Board")
    println(firstBoard.displayBoardWithScore())

    println("Player 2 Board")
    println(secondBoard.displayBoardWithScore())
    println(System.currentTimeMillis() - time)
    System.exit(1)
  }

}
