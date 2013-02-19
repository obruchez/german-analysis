package org.bruchez.german

object German {
  def main(args: Array[String]) {
    if (args.size != 1) {
      println("File needed")
      for { a <- args } { println(a) }
      return
    }

    val lines = CSV.parse(scala.io.Source.fromFile(args(0)).mkString+"\n")

    val trimmedLines = for (line <- lines) yield {
      for (cell <- line) yield cell.trim
    }

    val answers = Answer.answersFromLines(trimmedLines)

    //for (answer <- answers) { answer.dump(); println("===") }

    println("Réponses: "+answers.size)
    println()

    //Answer.keys.foreach(kv => println(" * "+kv._1+" ("+kv._2.map(_.toString).reduceLeft(_+", "+_)+")"))

    val totals = Answer.totals(answers)

    // @todo display values in same order as in answers

    println("Totaux:")
    for ((value, subTotals) <- totals.toSeq.sortBy(_._1)) {
      val total = subTotals.map(_._2).fold(0)(_ + _)
      val sortedSubTotals = subTotals.toSeq.sortBy(_._2).reverse
      val sortedSubTotalsAsString = Some(sortedSubTotals map { kv =>
        kv._1+" (%d, %.2f%%)".format(kv._2, 100.0 * kv._2.toDouble / total)
      }).filter(_.nonEmpty).map(_.reduceLeft(_+", "+_)).getOrElse("_")

      println(" - "+value+": "+sortedSubTotalsAsString)
    }
    println()
  }
}

case class Answer(
    number: Int,
    keyValues: Map[String, Seq[String]] = Map(),
    germanIs: Map[String, Int] = Map(),
    oralComprehension: Option[Int] = None,
    writtenComprehension: Option[Int] = None,
    oralExpression: Option[Int] = None,
    writtenExpression: Option[Int] = None,
    grammar: Option[Int] = None,
    books: Option[Int] = None) {
  def dump() {
    println("Numéro: "+number)
    println("Compréhension orale: "+oralComprehension.getOrElse("-"))
    println("Compréhension écrite: "+writtenComprehension.getOrElse("-"))
    println("Expression orale: "+oralExpression.getOrElse("-"))
    println("Expression écrite: "+writtenExpression.getOrElse("-"))
    println("Grammaire: "+grammar.getOrElse("-"))
    println("Livres: "+books.getOrElse("-"))
    println("L'allemand, c'est: "+Some(germanIs).filter(_.nonEmpty).map(_.map(kv => kv._1+" ("+kv._2+")").reduceLeft(_+", "+_)).getOrElse("-"))
    println("Valeurs:")
    for ((key, values) <- keyValues) {
      println(" - "+key+": "+Some(values).filterNot(_.isEmpty).map(_.reduceLeft(_+", "+_)).getOrElse("-"))
    }
  }
}

object Answer {
  val lineCount = 123

  //val keys = collection.mutable.Map[String, collection.mutable.Set[Int]]()

  def apply(lines: Seq[Seq[String]]): Answer = {
    @scala.annotation.tailrec
    def answerFromLines(remainingLines: Seq[Seq[String]], answer: Answer): Answer = {
      val key = remainingLines(0)(1)

      def score: Option[Int] = (2 to 7).find(column => remainingLines(0)(column).nonEmpty).map(_ - 1)

      /*def checkValues() {
        val notOnes =
          (for {
            (cell, index) <- remainingLines(1).zipWithIndex
            if remainingLines(0)(index).nonEmpty
          } yield (cell != "" && cell != "1")).fold(false)(_ || _)
        if (notOnes) {
          keys.getOrElseUpdate(key, collection.mutable.Set[Int]()).add(answer.number)
        }
      }*/

      val (newAnswer, linesToDrop) =
        if (key == "Compétences") {
          (answer, 1)
        } else if (key == "Comprendre discours") {
          (answer.copy(oralComprehension = score), 1)
        } else if (key == "Comprendre un texte") {
          (answer.copy(writtenComprehension = score), 1)
        } else if (key == "Parler") {
          (answer.copy(oralExpression = score), 1)
        } else if (key == "Ecrire") {
          (answer.copy(writtenExpression = score), 1)
        } else if (key == "Faire de la grammaire") {
          (answer.copy(grammar = score), 1)
        } else if (key == "Lire des livres") {
          (answer.copy(books = score), 2)
        } else if (key == "Allemand c'est") {
          val germanIs = remainingLines(0).drop(2).zipWithIndex map { case (value, index) =>
            (value, remainingLines(1)(2 + index))
          } filter { case (value, count) =>
            value.nonEmpty && count.nonEmpty
          } map { case (value, count) =>
            (value, count.toInt)
          }

          (answer.copy(germanIs = Map(germanIs: _*)), 3)
        } else {
          val values = remainingLines(0).drop(2).zipWithIndex map { case (value, index) =>
            (value, remainingLines(1)(2 + index))
          } filter { case (value, count) =>
            value.nonEmpty && count.nonEmpty
          } flatMap { case (value, count) =>
            List.fill(count.toInt)(value)
          }

          (answer.copy(keyValues = answer.keyValues + (key -> values)), 3)
        }

      if (remainingLines.size <= linesToDrop)
        newAnswer
      else
        answerFromLines(remainingLines.drop(linesToDrop), newAnswer)
    }

    answerFromLines(lines.drop(1), Answer(number = lines(0)(0).toInt))
  }

  @scala.annotation.tailrec
  def answersFromLines(remainingLines: Seq[Seq[String]], parsedAnswers: Seq[Answer] = Seq()): Seq[Answer] = {
    if (remainingLines.size < Answer.lineCount || !isHeaderLine(remainingLines.head))
      parsedAnswers
    else
      answersFromLines(
        remainingLines.drop(Answer.lineCount),
        parsedAnswers :+ Answer(remainingLines.take(Answer.lineCount)))
  }

  def cellsEmpty(line: Seq[String]): Boolean = line.map(_.isEmpty).fold(true)(_ && _)
  def isHeaderLine(line: Seq[String]): Boolean = line(0).map(_.isDigit).fold(true)(_ && _) && cellsEmpty(line.tail)

  def totals(answers: Seq[Answer]): Map[String, Map[String, Int]] = {
    val mutableTotals = collection.mutable.Map[String, collection.mutable.Map[String, Int]]()

    for (answer <- answers) {
      for ((key, values) <- answer.keyValues; value <- values) {
        val keyMap = mutableTotals.getOrElse(key, collection.mutable.Map[String, Int]())
        mutableTotals.put(key, keyMap)

        val valueCount = keyMap.getOrElse(value, 0)
        keyMap.put(value, valueCount + 1)
      }
    }

    // @todo germanIs + scores

    mutableTotals.toMap.map(kv => kv._1 -> kv._2.toMap)
  }
}
