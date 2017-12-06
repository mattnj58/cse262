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
    val file = Source.fromFile("/src/main/top40.sql").getLines.map(line => line.split(" ' ")).toList
    val numone = file(4).map(line=>line.split("\\s+")).toList
    val song = (file(1), file(3).map(line => line.split("\\s+")), if(numone(1)==0) false else true)


    /* used to see if I can read in
    val filename = "main/src/top40.sql"
    filename.foreach(println(_))
  }
  */

    //the multiplication function
    def *(int1: Int, int2: Int): Int ={
      val prod = int1 * int2
      prod
    }

    // the division function
    //should not work if the denominator is 0
    def /( numer: Int, denom: Int) = {
      if(denom!=0) {
        val divide = numer / denom
        divide
      } else {
        System.exit(0)
      }
    }

    //the addition method
    def +(int1: Int, int2: Int) = {
      val sum = int1 + int2
    }

    //the subtraction method takes the second number from the first
    def -(int1:Int, int2: Int) = {
      val sub = int1 - int2
    }
  }
}
