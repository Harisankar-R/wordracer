package com.codingnirvana.wordracer


case class Result(position: Int, letter: Char)

trait WordRacer {
  def initGameBoard(letter: Char)
  def pickLetter: Result
  def pickPosition(letter: Char): Int
}
