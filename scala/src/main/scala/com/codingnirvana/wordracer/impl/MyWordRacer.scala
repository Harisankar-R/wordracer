package com.codingnirvana.wordracer.impl

import com.codingnirvana.wordracer.{Result, WordRacer}

import scala.util.Random
import scala.util.matching.Regex

case class CharToCellMap(char: Char, cellId: Int)

class MyWordRacer(words: IndexedSeq[String]) extends WordRacer {

  var board: Board = Board(IndexedSeq.fill(7, 7)('*'), words)

  var fixedWords = Set.empty[Result]

  var freeCells: Set[Int] = Set.empty[Int]

  var possibleColumnWords = List.empty[List[IndexedSeq[CharToCellMap]]]

  val random = new Random

  def updateFixedWords(result: Result) = {
    fixedWords -= result
    result
  }

  def updateFreeCells(value: Int): Unit = freeCells -= value

  def searchFixedWords(sevenLetterWords: IndexedSeq[String], fiveLetterWords: IndexedSeq[String], i: Int = 0): List[String] = {
    if (i == 10) List.fill(2)(fiveLetterWords(random.nextInt(fiveLetterWords.length))) ::: List.fill(2)(sevenLetterWords(random.nextInt(sevenLetterWords.length)))
    else {
      val (firstChoice, secondChoice) = (sevenLetterWords(random.nextInt(sevenLetterWords.length)), sevenLetterWords(random.nextInt(sevenLetterWords.length)))
      val firstPrefix = firstChoice(0).toString + secondChoice(0).toString
      val secondPrefix = firstChoice(1).toString + secondChoice(1).toString
      val firstPrefixMatch = sevenLetterWords.find(_.substring(0, 2) == firstPrefix).getOrElse("")
      val secondPrefixMatch = sevenLetterWords.find(_.substring(0, 2) == secondPrefix).getOrElse("")
      if (firstPrefixMatch.isEmpty || secondPrefixMatch.isEmpty) searchFixedWords(sevenLetterWords, fiveLetterWords, i + 1)
      else List(firstChoice, secondChoice, firstPrefixMatch, secondPrefixMatch)
    }
  }

  def fixWords: Set[Result] = {
    val dict = words.sortBy(_.length)(Ordering.Int.reverse)
    val sevenLetterWords = dict.filter(_.length == 7)
    val fiveLetterWords = dict.filter(_.length == 5)
    val fixedWords = searchFixedWords(sevenLetterWords, fiveLetterWords)
    val firstColumn = fixedWords(0).zipWithIndex.map(x => (x._1, x._2 * 7)).toSet
    val secondColumn = fixedWords(1).zipWithIndex.map(x => (x._1, x._2 * 7 + 1)).toSet
    val firstRow = fixedWords(2).zipWithIndex.map(x => (x._1, x._2)).toSet
    val secondRow = fixedWords(3).zipWithIndex.map(x => (x._1, x._2 + 7)).toSet
    (firstColumn ++ secondColumn ++ firstRow ++ secondRow).map(element => Result(element._2, element._1))
  }

  def setFreeCells = {
    val cellsWithIndex = fixedBoard.cells.map(_.zipWithIndex).zipWithIndex
    freeCells = (for {
      (chars, ind) <- cellsWithIndex
      char <- chars.filter(_._1 == '*')
    } yield {
      ind * 7 + char._2
    }).toSet
  }

  def fixedBoard: Board = fixedWords.foldLeft(board)((acc, res) => acc.update(res.position / 7, res.position % 7, res.letter))

  def makeAllRegex(str: String): List[Regex] = {
    if (str.forall(_ == '*')) List.empty[Regex]
    else makeRegex(str) :: makeAllRegex(str.tail)
  }

  def makeRegex(str: String): Regex = str.map(char => if (char == '*') "[A-Z]{1}" else char).mkString("").r

  val allColumnsRegex: IndexedSeq[List[Regex]] = {
    val columns = fixedBoard.allColumns()
    columns.map(makeAllRegex)
  }

  def possibleWordsForColumn(col: Int): List[IndexedSeq[CharToCellMap]] = {
    val colRegex = allColumnsRegex(col)
    val possibleWords = (r: Regex) => words.map(r.findAllMatchIn(_)).filter(_.nonEmpty).map(_.mkString(""))
    val unfilled = fixedBoard.allColumns()(col).count(_ == '*')
    val allPossibleWords = colRegex.flatMap(possibleWords).filter(_.nonEmpty) ::: words.filter(_.length == unfilled).toList
    val toCellId = (word: String) => word.zipWithIndex.map(e => CharToCellMap(e._1, (7 - word.length + e._2) * 7 + col))
    allPossibleWords.map(toCellId)
  }

  def updatePossibleColumnWords(col: Int, value: CharToCellMap): Unit = {
    val updatedColumnWords = possibleColumnWords(col).filter(e => e.contains(value)).filter(_.nonEmpty)
    possibleColumnWords = possibleColumnWords.updated(col, updatedColumnWords)
  }

  def fromDeadColumns(col: Int = 0): Option[Int] = {
    if (col == 7) None
    else if (possibleColumnWords(col).isEmpty) freeCells.find(_ % 7 == col)
    else fromDeadColumns(col + 1)
  }


  def pickRandom(letter: Option[Char] = None): Result = {
    val pos = fromDeadColumns()
    //if no char can be placed or selected, always select 'E'
    val char = letter.getOrElse('E')
    val cellId = pos.getOrElse(freeCells.toList.max)
    updateFreeCells(cellId)
    updatePossibleColumnWords(cellId % 7, CharToCellMap(char, cellId))
    Result(cellId, char)
  }

  def chooseFromColumnWords(col: Int = 0, letter: Option[Char] = None): Result = {
    def chooseFromColumnWordHelper(candidate: List[IndexedSeq[CharToCellMap]]) = {
      if (candidate.isEmpty) chooseFromColumnWords(col + 1, letter)
      else {
        val selection = letter match {
          case None => candidate.head.head
          case Some(value) => candidate.head.filter(_.char == value).head
        }
        updatePossibleColumnWords(col, selection)
        updateFreeCells(selection.cellId)
        Result(selection.cellId, selection.char)
      }
    }

    if (col == 7) pickRandom(letter)
    else if (possibleColumnWords(col).isEmpty) chooseFromColumnWords(col + 1, letter)
    else letter match {
      case None =>
        val candidate = possibleColumnWords(col).map(_.filter(e => freeCells.contains(e.cellId))).filter(_.nonEmpty)
        chooseFromColumnWordHelper(candidate)
      case Some(value) =>
        val candidate = possibleColumnWords(col).map(_.filter(e => e.char == value && freeCells.contains(e.cellId))).filter(_.nonEmpty)
        chooseFromColumnWordHelper(candidate)
    }
  }

  def selectFromFixedWord: Option[Result] = fixedWords.headOption.map(updateFixedWords)

  def selectFromFixedWord(letter: Char): Option[Result] = fixedWords.find(_.letter == letter).map(updateFixedWords)

  override def initGameBoard(letter: Char): Unit = {
    board = board.update(3, 3, letter)
    fixedWords = fixWords
    setFreeCells
    possibleColumnWords = (0 to 6).map(possibleWordsForColumn).toList
  }

  override def pickLetter: Result = selectFromFixedWord.getOrElse(chooseFromColumnWords())

  override def pickPosition(letter: Char): Int = {
    val selection = selectFromFixedWord(letter).getOrElse(chooseFromColumnWords(letter = Some(letter)))
    selection.position
  }

}
