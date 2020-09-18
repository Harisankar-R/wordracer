package com.codingnirvana.wordracer.impl

import com.codingnirvana.wordracer.{Result, WordRacer}

import scala.util.Random
import scala.util.matching.Regex

class enhanced(words: IndexedSeq[String]) extends WordRacer {

  var board: Board = Board(IndexedSeq.fill(7, 7)('*'), words)

  var fixedWords = Set.empty[Result]

  var freeCells: Set[Int] = Set.empty[Int]

  var possibleColumnWords = List.empty[List[IndexedSeq[CharToCellMap]]]

  val random = new Random

  def updateFixedWords(result: Result): Unit = fixedWords -= result

  def updateFreeCells(value: Int): Unit = freeCells -= value

  def preFixRows = {
    val dict = words.sortBy(_.length)(Ordering.Int.reverse)
    val sevenLetterWords = dict.filter(_.length == 7)
    val fiveLetterWords = dict.filter(_.length == 5)

    def search(i: Int = 0): List[String] = {
      if (i == 10) List.fill(2)(sevenLetterWords(random.nextInt(sevenLetterWords.length))) ::: List.fill(2)(fiveLetterWords(random.nextInt(fiveLetterWords.length)))
      else {
        val (first, second) = (sevenLetterWords(random.nextInt(sevenLetterWords.length)), sevenLetterWords(random.nextInt(sevenLetterWords.length)))
        val prefix1 = first(0).toString + second(0).toString
        val prefix2 = first(1).toString + second(1).toString
        val firstRow = if (sevenLetterWords.exists(_.substring(0, 2) == prefix1)) sevenLetterWords.filter(_.substring(0, 2) == prefix1).head else ""
        val secondRow = if (sevenLetterWords.exists(_.substring(0, 2) == prefix2)) sevenLetterWords.filter(_.substring(0, 2) == prefix2).head else ""
        if (firstRow == "" || secondRow == "") search(i + 1)
        else List(first, second, firstRow, secondRow)
      }
    }

    val choice = search()
    val c1 = choice(0).zipWithIndex.map(x => (x._1, x._2 * 7)).toSet
    val c2 = choice(1).zipWithIndex.map(x => (x._1, x._2 * 7 + 1)).toSet
    val r1 = choice(2).zipWithIndex.map(x => (x._1, x._2)).toSet
    val r2 = choice(3).zipWithIndex.map(x => (x._1, x._2 + 7)).toSet
    println((c1 ++ c2 ++ r1 ++ r2).size)
    choice.foreach(println)
    (c1 ++ c2 ++ r1 ++ r2)
  }

  def fixedBoard: Board = fixedWords.foldLeft(board)((acc, res) => acc.update(res.position / 7, res.position % 7, res.letter))

  val createColumnRegex: IndexedSeq[List[Regex]] = {
    val columns = fixedBoard.allColumns()
    columns.map(makeAllRegex)
  }

  def makeAllRegex(str: String): List[Regex] = {
    //    if (str.forall(_ == '*')) List(makeRegex(str))
    if (str.forall(_ == '*')) List.empty[Regex]
    else makeRegex(str) :: makeAllRegex(str.tail)
  }

  def makeRegex(str: String): Regex = str.map(c => if (c == '*') "[A-Z]{1}" else c).mkString("").r

  def possibleWordsForColumn(c: Int): List[IndexedSeq[CharToCellMap]] = {
    val colRegex = createColumnRegex(c)
    val possibleWords = (r: Regex) => words.map(r.findAllMatchIn(_)).filter(_.nonEmpty).map(_.mkString(""))
    val starlength = fixedBoard.allColumns()(c).count(_ == '*')
    val allPossibleWords = colRegex.flatMap(possibleWords).filter(_.nonEmpty) ::: words.filter(_.length == starlength).toList
    val toGridId = (word: String) => word.zipWithIndex.map(e => CharToCellMap(e._1, (7 - word.length + e._2) * 7 + c))
    allPossibleWords.map(toGridId)
  }


  def updatePossibleColumnWords(c: Int, value: CharToCellMap): Unit = {
    val updatedColumnWords = possibleColumnWords(c).filter(e => e.contains(value)).filter(_.nonEmpty)
    possibleColumnWords = possibleColumnWords.updated(c, updatedColumnWords)
  }

  def fromDeadColumns(c: Int = 0): Option[Int] = {
    if (c == 7) {
      println("Picking random free cell")
      None
    }
    else if (possibleColumnWords(c).isEmpty) {
      val x = freeCells.find(_ % 7 == c)
      println(s"from dead column $c choosing free cell $x")
      x
    }
    else fromDeadColumns(c + 1)
  }


  def pickRandom(letter: Option[Char] = None): Result = {
    val pos = fromDeadColumns()
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
        println(s"Choosing from column $selection")
        Result(selection.cellId, selection.char)
      }
    }

    if (col == 7) pickRandom(letter)
    else if (possibleColumnWords(col).isEmpty) chooseFromColumnWords(col + 1, letter)
    else letter match {
      case None =>
        println("Picking from column words")
        val candidate = possibleColumnWords(col).map(e => e.filter(x => freeCells.contains(x.cellId))).filter(_.nonEmpty)
        //        println("Candidates: "+ candidate)
        chooseFromColumnWordHelper(candidate)
      case Some(value) =>
        println("Picking from column words")
        val candidate = possibleColumnWords(col).map(e => e.filter(x => x.char == value && freeCells.contains(x.cellId))).filter(_.nonEmpty)
        //        println("Candidates: "+ candidate)
        chooseFromColumnWordHelper(candidate)
    }
  }

  def selectFromFixedWord: Option[Result] = {
    if (fixedWords.isEmpty) None
    else {
      val selection = fixedWords.head
      updateFixedWords(selection)
      println("Selecting from fixed words: " + selection)
      Some(selection)
    }
  }

  def selectFromFixedWord(letter: Char): Option[Result] = {
    if (fixedWords.exists(_.letter == letter)) {
      val selection = fixedWords.filter(_.letter == letter).head
      updateFixedWords(selection)
      println("Selecting from fixed words: " + selection)
      Some(selection)
    }
    else None
  }

  override def initGameBoard(letter: Char): Unit = {
    board = board.update(3, 3, letter)
    fixedWords = preFixRows.map(e => Result(e._2, e._1))
    val cellsWithIndex = fixedBoard.update(3, 3, letter).cells.map(_.zipWithIndex).zipWithIndex
    freeCells = (for {
      (chars, ind) <- cellsWithIndex
      char <- chars
    } yield {
      if (char._1 == '*') ind * 7 + char._2 else -1
    }).filterNot(_ == -1).toSet
    possibleColumnWords = (0 to 6).map(possibleWordsForColumn).toList
  }

  override def pickLetter: Result = {
    println("-----------------Picking Letter-----------------")
    selectFromFixedWord.getOrElse(chooseFromColumnWords())
  }

  override def pickPosition(letter: Char): Int = {
    println("-----------------Picking postition-----------------")
    val selection = selectFromFixedWord(letter).getOrElse(chooseFromColumnWords(letter = Some(letter)))
    selection.position
  }


}
