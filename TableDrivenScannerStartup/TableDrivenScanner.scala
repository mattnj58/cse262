import com.sun.javafx.fxml.expression.Expression.Parser.Token

/*
  CSE 262 - Assignment 2
  Description: Table driven scanner for the calculator language in PLP, 4th Ed
  Author:
  Email:
  Date: September 2017
*/

// List of tokens to be recognized
case object lparen
case object rparen
case object plus
case object minus
case object times
case object div
case object assign
case object read    // keyword
case object write   // keyword
case class Id(i:String)
case class Number(n:String)

case object skip
case object error
case object eof

abstract class Action
case object Move extends Action
case object Recognize extends Action
case object Error extends Action

object TableDrivenScanner extends App {
 // Special chars
  val newline = '\n'
  val EOF = '$'

  // Character categories/sets
  val whitespace = Set(' ', '\t', '\n')
  val digits = ('0' to '9').toSet
  val letters = ('A' to 'Z').toSet ++ ('a' to 'z').toSet
  val punctuation = Map('(' -> lparen, ')' -> rparen, '+' -> plus,
                      '-' -> minus, '*' -> times)
  type State = Int
  type Token = Int
  case class ScanTabCell(var action:Action, var new_state:State)

  // Read in entire test file into a single string
  val source = io.Source.fromFile("testexpr.txt").getLines.toList.mkString("\n")
  println(source)
  println

  // Method to return a single char
  var i = -1
  def readchar = { i += 1; if (i < source.length) source(i) else EOF }
  def unreadchars(n:Int) = { i -= n }

  // Read in scan_tab and token_tab from a file
  // Fill in code to read in the tables from table.txt

  State := 0 //number of states
  Token := 0 //number of tokens

  // Main method (called by the parser) to get the next token
  def nexttoken = {

      // The code for the algorithm in Fig 2.11 goes in here


  }

  // Test program
  var t = nexttoken
  while (t != eof) {
      println(t)
      t = nexttoken
  }
}
