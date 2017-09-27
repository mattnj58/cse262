import scala.io.Source

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
  val keyword_tab = Map("read" -> read, "write" -> write)

  type State = Int
  type Token = Int
  case class ScanTabCell(var action:Action, var new_state:State)

  // Read in entire test file into a single string
  val source = io.Source.fromFile("src/main/testexpr.txt").getLines.toList.mkString("\n")
  println(source)
  println

  // Method to return a single char
  var i = -1
  def readchar = { i += 1; if (i < source.length) source(i) else EOF }
  def unreadchars(n:Int) = { i -= n }

  // Read in scan_tab and token_tab from a file
  var scan_tab = Array.ofDim[Int](19, 16)
  var token_tab = new Array[String](19)
  // Fill in code to read in the tables from table.txt

//  val lines = Source.fromFile("src/main/table.txt").getLines().toList
  var tokenIndex = 0
  var state:State = 1
//  var action:Action = null
  for(row <- Source.fromFile("src/main/table.txt").getLines()){
    token_tab(tokenIndex)=(row.length-1).toString
    var cell = row.split("\\s+")
    for(scancell <- cell){
      if(scancell.toInt>0){
        state = scancell.toInt
        //action = Move
        ScanTabCell(Move, state)
      } else if(scancell.toInt==0){
        state = scancell.toInt
        ScanTabCell(Recognize,state)
      }
    }
    tokenIndex= tokenIndex +1
  }

  /*
  for(line <- 0 to 14) {
    var row = lines(line).split("\\s+").map(cell => cell.toInt).toList

    //puts the token into token_tab and the rest of the table into the scan_tab
    token_tab(line)=row.last
    for(innerloop <- 0 until 16){
      scan_tab(line)(innerloop) = row(innerloop)
      print(scan_tab(line)(innerloop) + " ")
    }
    println("")
  }

  def scan_tab(char: Char, state: State): State ={

  }
  */


  def scan_tab(ch:Char, state: State):ScanTabCell ={
    ScanTabCell(Move,state)
  }

  // Main method (called by the parser) to get the next token
  def nexttoken = {

    // The code for the algorithm in Fig 2.11 goes in here

    var cur_char:Char = null
    var rememebered_char:List[Char]=null

    //checks if there is a comment string to ignore.... needs to implement the loop
    var cur_state:State = 1
    var image:String = null
    var remembered_state: State = 0

    //placeholder loop
    while(source!= EOF){
     var currentState: State = cur_state
     var image: String = null
     remembered_state = 0
      //place holder loop
     while(source != EOF){
       scan_tab(cur_char, cur_state).action match{
         case Move => if(cur_char != 0) {
         }
       }
     }
    }
  }

  // Test program
  var t = nexttoken
  while (t != eof) {
    println(t)
    t = nexttoken
  }
}
