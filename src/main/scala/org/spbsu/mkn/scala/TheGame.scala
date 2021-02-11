package org.spbsu.mkn.scala

import scala.io.StdIn.readLine
import scala.util.Random

object TheGame {

  sealed trait GuessResult

  case class Correct(numTries: Int) extends GuessResult

  case class Incorrect(bulls: Int, cows: Int) extends GuessResult

  class RepeatingDigitsException extends RuntimeException

  class WrongNumberLengthException(expected: Int, got: Int) extends RuntimeException

  private val symbols = ('0' to '9').mkString + ('A' to 'Z').mkString

  def generateNumberString(length: Int): String = Random.shuffle(symbols).toString().take(length)

  def findBulls(str1: String, str2: String): Int = {
    val prs = str2.zipWithIndex
    str1.zipWithIndex.count(pr => prs.contains(pr))
  }

  def validate(secret: String, userInput: String, numTries: Int = 1): GuessResult = if (secret.length != userInput.length)
    throw new WrongNumberLengthException(secret.length, userInput.length)
  else if (secret.toSet.size != secret.length)
    throw new RepeatingDigitsException
  else if (secret == userInput)
    Correct(numTries)
  else {
    val bulls = findBulls(secret, userInput)
    Incorrect(findBulls(secret, userInput), secret.count(c => userInput.contains(c)) - bulls)
  }


  def main(args: Array[String]): Unit = {
    println("Input the length of the keyword")
    val len = readLine.toInt
    val word = generateNumberString(len)
    println("Keyword is generated")

    var counter = 1;
    while (true) {
      print("Input you guess: ")
      val guess = readLine()
      if (!guess.toList.forall(c => symbols.contains(c)))
        println("Only numbers and letters are allowed")
      try {
        val result = validate(word, guess, counter)
        result match {
          case x: Correct =>
            println("Success! You spend " + x.numTries + " tries!")
            return
          case x: Incorrect =>
            println("Bulls: " + x.bulls + " | Cows: " + x.cows)
            counter = counter + 1
          case x =>
            println("Something went wrong");
            println(x);
        }
      }
      catch {
        case e: Exception => println(e)
      }
    }
  }
}
