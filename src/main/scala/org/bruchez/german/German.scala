package org.bruchez.german

object German {
  def main(args: Array[String]) {
    if (args.size != 1) {
      println("File needed")
      for { a <- args } { println(a) }
      return
    }

    val lines = CSV.parse(scala.io.Source.fromFile(args(0)).mkString+"\n")
    val fixedValues = Map("oui" -> "Oui", "non" -> "Non")
    val trimmedLines =
      for (line <- lines) yield {
        for (cell <- line) yield {
          val trimmed = cell.trim
          fixedValues.get(trimmed).getOrElse(trimmed)
        }
      }

    implicit val keys = Key.keys(lines)

    val answers = Answer.answersFromLines(trimmedLines)

    //for (answer <- answers) { answer.dump(); println("===") }

    println("Réponses: "+answers.size)
    println()

    //Answer.keys.foreach(kv => println(" * "+kv._1+" ("+kv._2.map(_.toString).reduceLeft(_+", "+_)+")"))

    println("Totaux:")
    Answer.dumpTotals(Answer.totals(answers))
    println()
    println()

    val germanUsefulYes = Answer.filteredAnswers(answers, "Utile d'apprendre l'allemand", "Oui")
    val germanUsefulNo = Answer.filteredAnswers(answers, "Utile d'apprendre l'allemand", "Non")
    def dumpGermanUseful(answers: Seq[Answer]) {
      Answer.dumpTotals(Answer.totals(
        Answer.filteredAnswers(answers, Set("Pourquoi")),
        withGermanIs = false,
        withCompetencies = false))
    }
    println("Allemand utile = oui:")
    dumpGermanUseful(germanUsefulYes)
    println("Allemand utile = non:")
    dumpGermanUseful(germanUsefulNo)
    println()
    println()

    val likeGermanYes = Answer.filteredAnswers(answers, "Aimez-vous l'allemand", "Oui")
    val likeGermanNo = Answer.filteredAnswers(answers, "Aimez-vous l'allemand", "Non")
    def dumpLikeGerman(answers: Seq[Answer]) {
      Answer.dumpTotals(Answer.totals(
        Answer.filteredAnswers(
          answers,
          Set("Participation en classe", "Faire travaux demandés", "Combien de temps pour apprentissage")),
        withGermanIs = false))
    }
    println("Aime l'allemand = oui:")
    dumpLikeGerman(likeGermanYes)
    println("Aime l'allemand = non:")
    dumpLikeGerman(likeGermanNo)
    println()
    println()
  }
}

case class Key(number: String, name: String)

object Key {
  def keys(lines: List[List[String]]): Seq[Key] =
    for (line <- lines.take(Answer.lineCount).filter(_(1).nonEmpty)) yield Key(" "*(4-line(0).length)+line(0), line(1))
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
        if (key == competenciesKey) {
          (answer, 1)
        } else if (key == oralComprehensionKey) {
          (answer.copy(oralComprehension = score), 1)
        } else if (key == writtenComprehensionKey) {
          (answer.copy(writtenComprehension = score), 1)
        } else if (key == oralExpressionKey) {
          (answer.copy(oralExpression = score), 1)
        } else if (key == writtenExpressionKey) {
          (answer.copy(writtenExpression = score), 1)
        } else if (key == grammarKey) {
          (answer.copy(grammar = score), 1)
        } else if (key == booksKey) {
          (answer.copy(books = score), 2)
        } else if (key == germanIsKey) {
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

  def resultsAsString(results: Map[String, Int]): String = {
    val total = results.map(_._2).fold(0)(_ + _)
    val sortedSubTotals = results.toSeq.sortBy(_._2).reverse

    Some(sortedSubTotals map { kv =>
      kv._1+" (%d, %.2f%%)".format(kv._2, 100.0 * kv._2.toDouble / total)
    }).filter(_.nonEmpty).map(_.reduceLeft(_+", "+_)).getOrElse("_")
  }

  def totals(
      answers: Seq[Answer],
      withKeyValues: Boolean = true,
      withGermanIs: Boolean = true,
      withCompetencies: Boolean = true): Map[String, Map[String, Int]] = {
    val mutableTotals = collection.mutable.Map[String, collection.mutable.Map[String, Int]]()

    for (answer <- answers) {
      def addKeyAndValue(key: String, value: String, count: Int = 1) {
        val keyMap = mutableTotals.getOrElse(key, collection.mutable.Map[String, Int]())
        mutableTotals.put(key, keyMap)

        val valueCount = keyMap.getOrElse(value, 0)
        keyMap.put(value, valueCount + count)
      }

      if (withKeyValues) {
        for ((key, values) <- answer.keyValues; value <- values) addKeyAndValue(key, value)
      }

      if (withGermanIs) {
        for ((value, count) <- answer.germanIs) addKeyAndValue(germanIsKey, value, count)
      }

      if (withCompetencies) {
        addKeyAndValue(oralComprehensionKey, answer.oralComprehension.map(_.toString).getOrElse(noAnswerValue))
        addKeyAndValue(writtenComprehensionKey, answer.writtenComprehension.map(_.toString).getOrElse(noAnswerValue))
        addKeyAndValue(oralExpressionKey, answer.oralExpression.map(_.toString).getOrElse(noAnswerValue))
        addKeyAndValue(writtenExpressionKey, answer.writtenExpression.map(_.toString).getOrElse(noAnswerValue))
        addKeyAndValue(grammarKey, answer.grammar.map(_.toString).getOrElse(noAnswerValue))
        addKeyAndValue(booksKey, answer.books.map(_.toString).getOrElse(noAnswerValue))
      }
    }

    mutableTotals.toMap.map(kv => kv._1 -> kv._2.toMap)
  }

  def dumpTotals(totals: Map[String, Map[String, Int]])(implicit keys: Seq[Key]) {
    for (key <- keys; subTotals <- totals.get(key.name)) {
      val valueCount = subTotals.map(_._2).sum
      val scoreAverage =
        if (scoreKeys.contains(key.name))
          " (moyenne: %.2f)".format(subTotals.map(kv => kv._1.toInt * kv._2).sum.toDouble / valueCount)
        else
          ""
      println(" - "+key.number+" "+key.name+" ("+valueCount+"): "+Answer.resultsAsString(subTotals)+scoreAverage)
    }
  }

  def filteredAnswers(answers: Seq[Answer], key: String, value: String): Seq[Answer] =
    answers.filter(_.keyValues(key).toSet.contains(value))

  def filteredAnswers(answers: Seq[Answer], keys: Set[String]): Seq[Answer] =
    answers.map(answer => answer.copy(keyValues = answer.keyValues.filter(kv => keys.contains(kv._1))))

  private val competenciesKey = "Compétences"
  private val oralComprehensionKey = "Comprendre discours"
  private val writtenComprehensionKey = "Comprendre un texte"
  private val oralExpressionKey = "Parler"
  private val writtenExpressionKey = "Ecrire"
  private val grammarKey = "Faire de la grammaire"
  private val booksKey = "Lire des livres"
  private val germanIsKey = "Allemand c'est"
  private val noAnswerValue = "Pas de réponse"

  private val scoreKeys =
    Set(oralComprehensionKey, writtenComprehensionKey, oralExpressionKey, writtenExpressionKey, grammarKey, booksKey)
}
