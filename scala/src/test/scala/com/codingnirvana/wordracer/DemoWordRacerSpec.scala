package com.codingnirvana.wordracer

import com.codingnirvana.wordracer.impl.Board
import org.scalatest.FlatSpec
import org.scalatest.Matchers._

class DemoWordRacerSpec extends FlatSpec {

  val stream = getClass.getResourceAsStream("/words.dat")
  val words = scala.io.Source.fromInputStream(stream).getLines.toIndexedSeq.sorted

  it should "calculate scores for a single row for a given length" in {
    val board = Board(null, words)
    val taken: Seq[Boolean] = Vector.fill(7)(false)
    board.calc("ABALONE", 0, 7, taken).scoreSoFar should be(13)
    board.calc("ABALONE", 0, 5, taken).scoreSoFar should be(5)
    board.calc("ABALONE", 0, 3, taken).scoreSoFar should be(4)
    board.calc("ABALONE", 0, 2, taken).scoreSoFar should be(2)
  }


  it should "calculate scores for Board 1" in {
    val cells = IndexedSeq(
      "ABALONE",
      "XIBIULA",
      "IQIPTIC",
      "SNOBWAY",
      "ELSAONM",
      "SHEBRAA",
      "WESYKYD")
      .map(_.toIndexedSeq)
    val board = Board(cells, words)
    val score = board.calculateScore()
    score.rowScore should be(Vector(13, 3, 2, 5, 3, 4, 1))
    score.colScore should be(Vector(8, 2, 13, 5, 13, 5, 3))
  }

  it should "calculate scores for Board 2" in {
    val cells = IndexedSeq(
      "FUTTOCK",
      "UNEQUAL",
      "TRAMMEL",
      "HOLYDAY",
      "OVERBID",
      "REBATES",
      "KNOZJJI")
      .map(_.toIndexedSeq)
    val board = Board(cells, words)
    val score = board.calculateScore()
    score.rowScore should be(Vector(13, 13, 13, 13, 13, 13, 1))
    score.colScore should be(Vector(13, 13, 4, 1, 0, 2, 1))
  }
}
