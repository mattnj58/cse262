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
    var scan_tab = Array[Array[State]]()
    var token_tab = new Array[String](19)
    // Fill in code to read in the tables from table.txt

    //reads in the file and puts the last thing in the string in token_tab
    var lineNumber = 0
    var colnumber = 1
    for(line <- Source.fromFile("src/main/table.txt").getLines()) {
      var row = line.split("\\s+")

      //puts the token in the table into the token_tab
      token_tab(lineNumber) = row.last

      //puts the rest of the table into the scan_tab
      var outloop = 0
      var innerloop = 0
      while(outloop < 18){
        while(innerloop < 15){
          scan_tab(outloop)(innerloop) = line(innerloop)
          colnumber= colnumber +1
          innerloop= innerloop+1
        }
        outloop = outloop+1
        lineNumber = lineNumber + 1

      }

    }

    var outloop=0
    var innerloop=0
    while(outloop<18){
      while(innerloop<15)
      {
        print(scan_tab(outloop)(innerloop))
        println(" ")
        innerloop = innerloop +1
      }
      outloop= outloop+1
    }

    // Main method (called by the parser) to get the next token
    def nexttoken = {

      // The code for the algorithm in Fig 2.11 goes in here
      var tok=0
      var cur_char=0
      var rememebered_char=0

      //repeat?
      var cur_state = 0
      var image:String = null
      var remembered_state: State = 0
      /*repeat
      while(source!= EOF){
       var currentState: State = cur_state
       var image: String = null
       remembered_state = 0
       while(source != EOF){

       }
      }*/
    }

    /*
    // Test program
    var t = nexttoken
    while (t != eof) {
      println(t)
      t = nexttoken
    }*/
  }
