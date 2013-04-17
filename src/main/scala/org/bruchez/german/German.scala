package org.bruchez.german

import java.io._

object German {
  def main(args: Array[String]) {
    if (args.size < 2) {
      println("File(s) needed")
      for { a <- args } { println(a) }
      return
    }

    val inFiles = args.init
    val outFile = args.last

    val out = new PrintWriter(new File(outFile), "UTF-8")

    try {
      inFiles.foreach(dumpFileResults(out, _))
      if (inFiles.size > 1) dumpAllFileResults(out, inFiles)
    } finally {
      out.close()
    }
  }

  def fileToClass(file: String): String = {
    val fileParts = file.split('.')
    if (fileParts.size <= 1) file else fileParts.init.mkString(".")
  }

  def dumpFileResults(out: PrintWriter, file: String) {
    dumpResults(out, fileToClass(file), CSV.parse(scala.io.Source.fromFile(file).mkString+"\n"))
  }

  def dumpAllFileResults(out: PrintWriter, files: Seq[String]) {
    val lines = files.map(file => CSV.parse(scala.io.Source.fromFile(file).mkString+"\n")).reduce(_ ++ _)
    dumpResults(out, "Toutes les classes ("+files.map(fileToClass).reduceLeft(_+" / "+_)+")", lines)
  }

  def dumpResults(out: PrintWriter, header: String, lines: List[List[String]]) {
    val stars = "*" * (header.length + 4)
    out.println(stars)
    out.println("* %s *".format(header))
    out.println(stars)
    out.println()

    val fixedValues = Map("oui" -> "Oui", "non" -> "Non")
    val trimmedLines =
      for (line <- lines) yield {
        for (cell <- line) yield {
          val trimmed = cell.trim
          fixedValues.get(trimmed).getOrElse(trimmed)
        }
      }

    implicit val keys = Key.keys(trimmedLines)
    //keys.foreach(key => out.println(key.number+" "+key.name))

    val answers = Answer.answersFromLines(trimmedLines)
    //answers.foreach(answer => { answer.dump(); out.println("===") })

    out.println("Réponses: "+answers.size)
    out.println()

    // Totals
    out.println("Totaux")
    out.println("------")
    out.println()
    Answer.dumpTotals(out, Answer.totals(answers))
    out.println()

    // Hypothesis 2
    val germanUsefulYes = Answer.filteredAnswers(answers, "Utile d'apprendre l'allemand", "Oui")
    val germanUsefulNo = Answer.filteredAnswers(answers, "Utile d'apprendre l'allemand", "Non")
    def dumpHypothesis2(answers: Seq[Answer]) {
      Answer.dumpTotals(out, Answer.totals(
        Answer.filteredAnswers(answers, Set("Pourquoi")),
        withCompetencies = false))
    }
    out.println("Hypothèse 2")
    out.println("-----------")
    out.println()
    out.println("Allemand utile = oui:")
    dumpHypothesis2(germanUsefulYes)
    out.println()
    out.println("Allemand utile = non:")
    dumpHypothesis2(germanUsefulNo)
    out.println()

    // Hypothesis 3 (part 1)
    def dumpCompetency(f: (Answer, Int) => Boolean, competency: String) {
      def dump(answers: Seq[Answer]) {
        Answer.dumpTotals(out, Answer.totals(
          Answer.filteredAnswers(
            answers,
            Set("Participation en classe", "Faire travaux demandés", "Combien de temps pour apprentissage")),
          withCompetencies = false))
      }

      val filteredAnswers13 = answers.filter(answer => f(answer, 1) || f(answer, 2) || f(answer, 3))
      out.println(competency+" = 1-3:")
      dump(filteredAnswers13)
      out.println()

      val filteredAnswers46 = answers.filter(answer => f(answer, 4) || f(answer, 5) || f(answer, 6))
      out.println(competency+" = 4-6:")
      dump(filteredAnswers46)
      out.println()

      for (score <- 1 to 6) {
        val filteredAnswers = answers.filter(answer => f(answer, score))
        if (filteredAnswers.nonEmpty) {
          out.println(competency+" = "+score+":")
          dump(filteredAnswers)
          out.println()
        }
      }
    }
    out.println("Hypothèse 3")
    out.println("-----------")
    out.println()
    dumpCompetency(_.oralComprehension == Some(_), "Comprendre discours")
    dumpCompetency(_.writtenComprehension == Some(_), "Comprendre un texte")
    dumpCompetency(_.oralExpression == Some(_), "Parler")
    dumpCompetency(_.writtenExpression == Some(_), "Ecrire")
    dumpCompetency(_.grammar == Some(_), "Faire de la grammaire")
    dumpCompetency(_.books == Some(_), "Lire des livres")

    // Hypothesis 3 (part 2)
    val likeGermanYes = Answer.filteredAnswers(answers, "Aimez-vous l'allemand", "Oui")
    val likeGermanNo = Answer.filteredAnswers(answers, "Aimez-vous l'allemand", "Non")
    def dumpHypothesis3(answers: Seq[Answer]) {
      Answer.dumpTotals(out, Answer.totals(
        Answer.filteredAnswers(
          answers,
          Set("Participation en classe", "Faire travaux demandés", "Combien de temps pour apprentissage"))))
    }
    out.println("Aime l'allemand = oui:")
    dumpHypothesis3(likeGermanYes)
    out.println()
    out.println("Aime l'allemand = non:")
    dumpHypothesis3(likeGermanNo)
    out.println()

    // Hypothesis 8
    val allImages =
      (for {
        answer <- answers
        images <- answer.keyValues.get("3 images").toSeq
        image <- images.map(_._1)
      } yield image).distinct.sorted
    out.println("Hypothèse 8")
    out.println("-----------")
    out.println()
    for (image <- allImages) {
      val answersForImage = Answer.filteredAnswers(answers, "3 images", image)
      out.println("Image = "+image+":")
      Answer.dumpTotals(out, Answer.totals(
        Answer.filteredAnswers(
          answersForImage,
          Set("Aimez-vous l'allemand")),
        withCompetencies = false))
    }
    out.println()

    // Hypothesis 9
    def dumpHypothesis9(answers: Seq[Answer]) {
      Answer.dumpTotals(out, Answer.totals(
        Answer.filteredAnswers(
          answers,
          Set("en dehors parle allemand/Ch-all", "Séjours", "Combien de temps", "Combien de temps (classes)")),
        withCompetencies = false))
    }
    out.println("Hypothèse 9")
    out.println("------------")
    out.println()
    out.println("Aime l'allemand = oui:")
    dumpHypothesis9(likeGermanYes)
    out.println()
    out.println("Aime l'allemand = non:")
    dumpHypothesis9(likeGermanNo)
    out.println()

    // Hypothesis 10
    def dumpHypothesis10(answers: Seq[Answer]) {
      Answer.dumpTotals(out, Answer.totals(
        Answer.filteredAnswers(answers, Set("Télévision en allemand", "Chansons en allemand")),
        withCompetencies = false))
    }
    out.println("Hypothèse 10")
    out.println("------------")
    out.println()
    out.println("Aime l'allemand = oui:")
    dumpHypothesis10(likeGermanYes)
    out.println()
    out.println("Aime l'allemand = non:")
    dumpHypothesis10(likeGermanNo)
    out.println()

    // Hypothesis 11
    def dumpHypothesis11(answers: Seq[Answer]) {
      Answer.dumpTotals(out, Answer.totals(
        Answer.filteredAnswers(answers, Set("Utilisation études/vie professionnelle")),
        withCompetencies = false))
    }
    out.println("Hypothèse 11")
    out.println("------------")
    out.println()
    out.println("Aime l'allemand = oui:")
    dumpHypothesis11(likeGermanYes)
    out.println()
    out.println("Aime l'allemand = non:")
    dumpHypothesis11(likeGermanNo)
    out.println()

    // Hypothesis 12
    val findUsefulYes = Answer.filteredAnswers(answers, "Utile d'apprendre l'allemand", "Oui")
    val findUsefulNo = Answer.filteredAnswers(answers, "Utile d'apprendre l'allemand", "Non")
    def dumpHypothesis12(answers: Seq[Answer]) {
      Answer.dumpTotals(out, Answer.totals(
        Answer.filteredAnswers(answers, Set("Aimez-vous l'allemand")),
        withCompetencies = false))
    }
    out.println("Hypothèse 12")
    out.println("------------")
    out.println()
    out.println("Utile d'apprendre l'allemand = oui:")
    dumpHypothesis12(findUsefulYes)
    out.println()
    out.println("Utile d'apprendre l'allemand = non:")
    dumpHypothesis12(findUsefulNo)
    out.println()
  }
}

case class Key(number: String, name: String)

object Key {
  def keys(lines: List[List[String]]): Seq[Key] = {
    val firstAnswer = lines.take(123)
    val linesWithPrevious = firstAnswer.zip(List("", "") :: firstAnswer.init)
    ((for ((line, previousLine) <- linesWithPrevious.filter(_._1(1).nonEmpty)) yield {
      val number = Some(line(0)).filter(_.nonEmpty).getOrElse(previousLine(0))
      val filteredNumber = if (line(1) == "Comprendre discours") "" else number
      Key(" "*(4-filteredNumber.length)+filteredNumber, line(1))
    }) map { key =>
      if (key.name == Answer.germanIsKey) {
        Seq(key) ++ Answer.germanIsValues.map(value => Key(" "*4, Answer.extendedGermanIsKey.format(value)))
      } else if (key.name == Answer.howMuchTime) {
        Seq(key, Key(" "*4, Answer.simplifiedHowMuchTime))
      } else {
        Seq(key)
      }
    }).flatten
  }
}

case class Answer(
    number: Int,
    keyValues: Map[String, Seq[(String, Int)]] = Map(),
    oralComprehension: Option[Int] = None,
    writtenComprehension: Option[Int] = None,
    oralExpression: Option[Int] = None,
    writtenExpression: Option[Int] = None,
    grammar: Option[Int] = None,
    books: Option[Int] = None) {
  def dump(out: PrintWriter) {
    out.println("Numéro: "+number)
    out.println("Compréhension orale: "+oralComprehension.getOrElse("-"))
    out.println("Compréhension écrite: "+writtenComprehension.getOrElse("-"))
    out.println("Expression orale: "+oralExpression.getOrElse("-"))
    out.println("Expression écrite: "+writtenExpression.getOrElse("-"))
    out.println("Grammaire: "+grammar.getOrElse("-"))
    out.println("Livres: "+books.getOrElse("-"))
    out.println("Valeurs:")
    for ((key, values) <- keyValues) {
      val valueCountStrings = values map { case (value, count) => value+" ("+count+")" }
      out.println(" - "+key+": "+Some(valueCountStrings).filterNot(_.isEmpty).map(_.reduceLeft(_+", "+_)).getOrElse("-"))
    }
  }
}

object Answer {
  //val keys = collection.mutable.Map[String, collection.mutable.Set[Int]]()

  def apply(lines: Seq[Seq[String]]): Answer = {
    @scala.annotation.tailrec
    def answerFromLines(remainingLines: Seq[Seq[String]], answer: Answer): Answer = {
      val key = if (remainingLines(0)(1).nonEmpty) remainingLines(0)(1) else remainingLines(1)(1)
      assert(key.nonEmpty)

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
        } else {
          def countFromString(value: String, count: String): Option[Int] = {
            // For "Allemand c'est" default count to 1 if not present
            if (key == germanIsKey && value == Answer.noAnswerValue)
              None
            else if (key == germanIsKey && count.isEmpty)
              Some(1)
            else
              Some(count).filterNot(_.isEmpty).map(_.toInt)
          }

          val values = remainingLines(0).drop(2).zipWithIndex map { case (value, index) =>
            (value, remainingLines(1)(2 + index))
          } filter { case (value, count) =>
            value.nonEmpty && countFromString(value, count).nonEmpty
          } map { case (value, count) =>
            (value, countFromString(value, count).get)
          }

          val baseKeyValues = answer.keyValues + (key -> values)
          val keyValues =
            if (key == germanIsKey)
              baseKeyValues ++ extendedGermanIsValues(values)
            else if (key == howMuchTime)
              baseKeyValues ++ simplifiedHowMuchTimeValues(values)
            else
              baseKeyValues

          (answer.copy(keyValues = keyValues), 3)
        }

      if (remainingLines.size <= linesToDrop)
        newAnswer
      else
        answerFromLines(remainingLines.drop(linesToDrop), newAnswer)
    }

    val numberAsString = if (lines(0)(0).toLowerCase.startsWith("sujet")) lines(0)(0).substring(6) else lines(0)(0)
    answerFromLines(lines.drop(1), Answer(number = numberAsString.toInt))
  }

  @scala.annotation.tailrec
  def answersFromLines(remainingLines: Seq[Seq[String]], parsedAnswers: Seq[Answer] = Seq()): Seq[Answer] = {
    val nextAnswerOption = remainingLines.zipWithIndex.find(li => li._1(0) == "4.8").map(_._2 + 2)

    nextAnswerOption match {
      case None => parsedAnswers
      case Some(nextAnswer) => {
        answersFromLines(
          remainingLines.drop(nextAnswer).dropWhile(_(0).isEmpty),
          parsedAnswers :+ Answer(remainingLines.take(nextAnswer)))
      }
    }
  }

  def extendedGermanIsValues(values: Seq[(String, Int)]): Seq[(String, Seq[(String, Int)])] =
    values map { case (value, count) =>
      // One key per original value
      extendedGermanIsKey.format(value) -> Seq((count.toString, 1))
    }

  def simplifiedHowMuchTimeValues(values: Seq[(String, Int)]): Seq[(String, Seq[(String, Int)])] = {
   val durationClasses =
    for (value <- values) yield {
      assert(values.size == 1)
      val duration = value._1
      val less = Set("1 semaine", "2 semaines").contains(duration)
      val more = Set("3 semaines", "1 mois", "2 mois", "3 mois", "4-6 mois", "plus de 6 mois").contains(duration)
      val durationClass =
        if (less) "Moins de 3 semaines"
        else if (more) "Plus de 3 semaines"
        else "Autre"
      (durationClass, 1)
    }

    Seq(simplifiedHowMuchTime -> durationClasses)
  }

  def cellsEmpty(line: Seq[String]): Boolean = line.map(_.isEmpty).fold(true)(_ && _)
  def isHeaderLine(line: Seq[String]): Boolean = line(0).map(_.isDigit).fold(true)(_ && _) && cellsEmpty(line.tail)

  def valuesAndCountsAsString(key: Key, valuesAndCounts: Map[String, (Int, Int)], answerCount: Int): String = {
    val sortedValuesAndCounts = valuesAndCounts.toSeq.sortBy(_._1).reverse.sortBy(_._2._1).reverse

    Some(sortedValuesAndCounts map { kv =>
      val percentageString =
        if (key.name == Answer.germanIsKey) {
          ""
        } else {
          val percentage = 100.0 * kv._2._2.toDouble / answerCount
          assert(percentage <= 100.0)
          ", %.2f%%".format(percentage)
        }
      kv._1+" (%d%s)".format(kv._2._1, percentageString)
    }).filter(_.nonEmpty).map(_.reduceLeft(_+", "+_)).getOrElse("-")
  }

  case class Totals(
    // First int value is total counting duplicate values, second int value is total counting duplicate values as 1
    totalsByKeyAndValue: Map[String, Map[String, (Int, Int)]],
    answerCount: Int)

  def totals(
      answers: Seq[Answer],
      withKeyValues: Boolean = true,
      withCompetencies: Boolean = true): Totals = {
    val mutableTotals = collection.mutable.Map[String, collection.mutable.Map[String, (Int, Int)]]()

    for (answer <- answers) {
      def addKeyAndValue(key: String, value: String, count: Int = 1) {
        val keyMap = mutableTotals.getOrElse(key, collection.mutable.Map[String, (Int, Int)]())
        mutableTotals.put(key, keyMap)

        val valueCount = keyMap.getOrElse(value, (0, 0))
        keyMap.put(value, (valueCount._1 + count, valueCount._2 + 1))
      }

      if (withKeyValues) {
        for ((key, values) <- answer.keyValues; value <- values) addKeyAndValue(key, value._1, value._2)
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

    Totals(mutableTotals.toMap.map(kv => kv._1 -> kv._2.toMap), answers.size)
  }

  def dumpTotals(out: PrintWriter, totals: Totals)(implicit keys: Seq[Key]) {
    def scoreAverage(key: Key, totalsByValue: Map[String, (Int, Int)]): String =
      if (scoreKeys.contains(key.name)) {
        val countsByScore = totalsByValue.filterNot(_._1 == "Pas de réponse") map { kv =>
          val countWithDuplicates = kv._2._1
          val countWithoutDuplicates = kv._2._2
          assert(countWithDuplicates == countWithoutDuplicates)
          kv._1.toInt -> countWithDuplicates
        }
        val totalScore = countsByScore.map(kv => kv._1 * kv._2).sum
        val totalScoreCount = countsByScore.map(_._2).sum
        val average = totalScore.toDouble / totalScoreCount
        " (moyenne: %.2f)".format(average)
    } else {
      ""
    }

    for (key <- keys; totalsByValue <- totals.totalsByKeyAndValue.get(key.name)) {
      val valueCountString =
        if (key.name == Answer.germanIsKey) {
          ""
        } else {
          val valueCount = totalsByValue.map(_._2._1).fold(0)(_ + _)
          " ("+valueCount+")"
        }

      out.println(
        " - "+key.number+" "+key.name+valueCountString+": "+
        Answer.valuesAndCountsAsString(key, totalsByValue, totals.answerCount)+
        scoreAverage(key, totalsByValue))
    }
  }

  def filteredAnswers(answers: Seq[Answer], key: String, value: String): Seq[Answer] =
    answers.filter(_.keyValues(key).map(_._1).toSet.contains(value))

  def filteredAnswers(answers: Seq[Answer], keys: Set[String]): Seq[Answer] =
    answers.map(answer => answer.copy(keyValues = answer.keyValues.filter(kv => keys.contains(kv._1))))

  val competenciesKey = "Compétences"
  val oralComprehensionKey = "Comprendre discours"
  val writtenComprehensionKey = "Comprendre un texte"
  val oralExpressionKey = "Parler"
  val writtenExpressionKey = "Ecrire"
  val grammarKey = "Faire de la grammaire"
  val booksKey = "Lire des livres"
  val germanIsKey = "Allemand c'est"
  val extendedGermanIsKey = "Allemand c'est (%s)"
  val howMuchTime = "Combien de temps"
  val simplifiedHowMuchTime = "Combien de temps (classes)"
  val noAnswerValue = "Pas de réponse"

  val scoreKeys = Set(
    oralComprehensionKey,
    writtenComprehensionKey,
    oralExpressionKey,
    writtenExpressionKey,
    grammarKey,
    booksKey)

  val germanIsValues = Seq(
    "intéressant",
    "ennuyeux",
    "facile",
    "difficile",
    "utile",
    "inutile",
    "choix",
    "obligation",
    "nouvelle culture",
    "parler aux autres",
    "L comme les autres",
    "pouvoir travailler à l'étranger")
}
