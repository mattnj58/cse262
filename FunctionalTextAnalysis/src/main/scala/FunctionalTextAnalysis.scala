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

    val file = Source.fromFile("src/main/top40.sql").getLines().toList
    file.foreach(line => println(line))

    /*
    val filename = "main/src/top40.sql"
    filename.foreach(println(_))
  }
  */

  }
}
