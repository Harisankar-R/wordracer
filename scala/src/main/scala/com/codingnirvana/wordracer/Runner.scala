package com.codingnirvana.wordracer

import java.util.Calendar

import com.codingnirvana.wordracer.impl.TestWordRacer

object Runner {

  def main(args: Array[String]) = {
    val stream = getClass.getResourceAsStream("/words.dat")
    val words = scala.io.Source.fromInputStream(stream).getLines.toIndexedSeq.sorted

    val now = Calendar.getInstance()
//    val wRacer = new MyWordRacer(words)
    val wRacer = new TestWordRacer(words)


    //    wRacer.initGameBoard(StdIn.readChar())
    wRacer.initGameBoard('A')

    //    val myTurn = StdIn.readChar() == '1'

    //    (0 until 48).foldLeft(myTurn) { case (turn, i) =>
    //      if (turn) {
    //        val result = wRacer.pickLetter
    //        println(result.letter + " " + result.position)
    //        Console.flush()
    //      } else {
    //        val letter = StdIn.readChar()
    //        val position = wRacer.pickPosition(letter)
    //        println(position)
    //        Console.flush()
    //      }
    //      !turn
    //    }

//    println(wRacer.fixedWords)
//    println(wRacer.fixedWords.size)
////    println(wRacer.fixedWords.toList.sortBy(e => e._2))
//    println(wRacer.fixedBoard)
//    println(wRacer.createColumnRegex)
////    println(wRacer.possibleWordsForColumn(0))
////    println(wRacer.possibleWordsForColumn(1))
////    println(wRacer.possibleWordsForColumn(2))
////    println(wRacer.possibleWordsForColumn(3))
//    println(wRacer.possibleWordsForColumn(4))
//    println(wRacer.possibleWordsForColumn(5))
//    println(wRacer.possibleWordsForColumn(6))


//    println((wRacer.preFixRows).size)
    println(wRacer.initGameBoard('A'))
//    println(x.size)
    System.exit(0)
  }

}
