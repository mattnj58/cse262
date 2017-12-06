/**
  * Matthew Wong
  * myw219@lehigh.edu
  * Assignment 5: Functional Text Analysis - takes in a file of the top 40 songs and checks for the probability
  * of getting #1
  *
  *
  */



import scala.io.Source

class FunctionalTextAnalysis {

  def main(args: Array[String]): Unit = {

    //reads in the top40 file and splits it into a tuple3
    val file = Source.fromFile("/src/main/top40.sql").getLines.map(line => line.split(" ' "))


    //val file = Source.fromFile("src/main/top40.sql").getLines.map(word => word.split(" ' ").toList).toList
    //file.foreach(line => println(line))

    //creates the tuple3 to store the song information
    /*
    def createTuple3 (artist: List[String], song: List[String], num1: Int) = {

    }
    */


    /* used to see if I can read in
    val filename = "main/src/top40.sql"
    filename.foreach(println(_))
  }
  */

  }
}
