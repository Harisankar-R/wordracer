package com.codingnirvana.wordracer.impl


import com.codingnirvana.wordracer.{Result, WordRacer}

import scala.collection.Searching._
import scala.util.{Random}

case class Score(rowScore: Seq[Int], colScore: Seq[Int]) {
  val totalScore = rowScore.sum + colScore.sum
}

case class ScoreState(scoreSoFar: Int, taken: Seq[Boolean]) {
  def +(other: ScoreState) = other.copy(scoreSoFar = scoreSoFar + other.scoreSoFar)
}


case class Board(cells: IndexedSeq[IndexedSeq[Char]], words: IndexedSeq[String]) {

  def update(result: Result): Board = this.update(result.position / 7, result.position % 7, result.letter)

  def update(r: Int, c: Int, letter: Char): Board = Board(cells.updated(r, cells(r).updated(c, letter)), words)

  def apply(r: Int)(c: Int) = cells(r)(c)

  override def toString = cells.map(_.mkString("  ")).mkString("\n")

  def displayBoardWithScore(): String = {
    val score = calculateScore()
    val rows = cells.map(_.mkString("   "))
      .zip(score.rowScore)
      .map { case (v, s) => s"$v   $s" }
      .mkString("\n")

    val lastRow = s"${score.colScore.map(_.toString.padTo(2, " ").mkString("")).mkString("  ")}  ${score.totalScore}"

    rows + "\n" + lastRow
  }

  val scoreMap = Seq(0, 0, 1, 2, 3, 5, 8, 13)

  def calculateScore() = Score(calcScore(cells), calcScore(cells.transpose))

  def calcScore(_cells: IndexedSeq[IndexedSeq[Char]]) = {
    _cells.map { cell =>
      val taken = Vector.fill(7)(false)
      val initialScoreState = ScoreState(0, taken)
      (2 to 7).reverse.foldLeft(initialScoreState) {
        case (scoreState, len) => scoreState + calc(cell.mkString, 0, len, scoreState.taken)
      }
    }.map(_.scoreSoFar)
  }

  def calc(rowOrCol: String, start: Int, len: Int, taken: Seq[Boolean]): ScoreState = {
    if (start + len > rowOrCol.length) {
      ScoreState(0, taken)
    } else {
      val wordToFind = rowOrCol.slice(start, start + len).mkString("")
      val alreadyTaken = taken.slice(start, start + len).contains(true)
      (alreadyTaken, words.search(wordToFind)) match {
        case (false, Found(x)) =>
          val updatedTaken = replaceRange(taken, start, start + len, true).toSeq
          ScoreState(scoreMap(len), taken) + calc(rowOrCol, start + len, len, updatedTaken)
        case _ => calc(rowOrCol, start + 1, len, taken)
      }
    }
  }

  def replaceRange[T](xs: Iterable[T], from: Int, until: Int, default: T) = {
    xs.zipWithIndex.map {
      case (x, index) => if (index >= from && index < until) default else x
    }
  }

  def allRows(): IndexedSeq[String] = (0 to 6).map(i => cells(i).mkString(""))

  def allColumns(): IndexedSeq[String] = {
    val t = cells.transpose
    (0 to 6).map(i => t(i).mkString(""))
  }

}

class DemoWordRacer(words: IndexedSeq[String]) extends WordRacer {

  var board = Board(IndexedSeq.fill(7, 7)('*'), words)

  val random = new Random(50)

  override def initGameBoard(letter: Char): Unit = {
    board = board.update(3, 3, letter)
  }

  override def pickLetter: Result = {
    val rand = ('A' + random.nextInt(26)).toChar
    Result(pickPosition(rand), rand)
  }

  override def pickPosition(letter: Char): Int = {
    val pos = random.nextInt(49)
    val row = pos / 7
    val col = pos % 7

    if ((row, col) == (3, 3)) {
      pickPosition(letter)
    } else if (board(row)(col) != '*') {
      pickPosition(letter)
    } else {
      board = board.update(row, col, letter)
      pos
    }
  }
}
