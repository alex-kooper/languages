/*
  Facebook Hacker Cup 2013 Qualification Round

  Your friend John uses a lot of emoticons when you talk to him on Messenger.
  In addition to being a person who likes to express himself through emoticons, he hates unbalanced parenthesis
  so much that it makes him go :(

  Sometimes he puts emoticons within parentheses, and you find it hard to tell if a parenthesis really
  is a parenthesis or part of an emoticon.

  A message has balanced parentheses if it consists of one of the following:

  - An empty string ""
  - One or more of the following characters: 'a' to 'z', ' ' (a space) or ':' (a colon)
  - An open parenthesis '(', followed by a message with balanced parentheses, followed by a close parenthesis ')'.
  - A message with balanced parentheses followed by another message with balanced parentheses.
  - A smiley face ":)" or a frowny face ":("

  Write a program that determines if there is a way to interpret his message while leaving the parentheses balanced.
*/

import cats.implicits._

import scala.annotation.tailrec

object BalancedSmileys extends App {

  /**
    * Tail recursive solution to balanced Smileys problem using linear algorithm
    * It keeps track of maximum and minimum numbers of open brackets
    */
  def isBalanced(s: String): Boolean = {

    @tailrec
    def go(minOpen: Int, maxOpen: Int, s: LazyList[Char]): Boolean = s match {
      case ':' #:: '(' #:: rest => go(minOpen, maxOpen + 1, rest)
      case ':' #:: ')' #:: rest => go(minOpen - 1, maxOpen, rest)
      case '(' #:: rest => go(minOpen + 1, maxOpen + 1, rest)
      case ')' #:: rest =>
        if(maxOpen == 0)
          false
        else
          go(minOpen - 1, maxOpen - 1, rest)
      case _ #:: rest => go(minOpen, maxOpen, rest)
      case _ => (minOpen to maxOpen).contains(0)
    }

    go(0, 0, LazyList.from(s))
  }

  /**
    * Linear algorithm implemented using monadic fold from Cats
    * It keeps track of maximum and minimum numbers of open brackets
    */
  def isBalancedM(s: String): Boolean = {
    case class State(afterColon: Boolean, minOpen: Int, maxOpen: Int)

    def processChar(currentState: State, c: Char): Option[State] = (currentState, c) match {
      case (state, ':') => Some(state.copy(afterColon = true))
      case (State(true, minOpen, maxOpen), '(') => Some(State(afterColon = false, minOpen, maxOpen + 1))
      case (State(true, minOpen, maxOpen), ')') => Some(State(afterColon = false, minOpen - 1, maxOpen))
      case (State(false, minOpen, maxOpen), '(') => Some(State(afterColon = false, minOpen + 1, maxOpen + 1))
      case (State(false, minOpen, maxOpen), ')') =>
        if(maxOpen == 0)
          None
        else
          Some(State(afterColon = false, minOpen - 1, maxOpen - 1))
      case (state, _) => Some(state)
    }

    LazyList.from(s)
      .foldM(State(afterColon = false, 0, 0))(processChar)
      .exists(state => (state.minOpen to state.maxOpen).contains(0))
  }

  val input = Seq(
    "hacker cup: started :):)",
    "(:)",
    "i am sick today (:()",
    ")(",
    ":(("
  )

  println("Testing isBalanced")
  println("__________________")

  input.foreach { s =>
    println(s"""isBalanced("$s") = ${isBalanced(s)}""")
  }

  println("\nTesting isBalancedM")
  println("___________________")

  input.foreach { s =>
    println(s"""isBalancedM("$s") = ${isBalancedM(s)}""")
  }
}
