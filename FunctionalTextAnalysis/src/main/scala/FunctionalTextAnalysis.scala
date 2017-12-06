/**
  * Matthew Wong
  * CSE 262
  * myw219@lehigh.edu
  * Assignment 5: Functional Text Analysis - takes in a file of the top 40 songs and checks for the probability
  * of getting #1
  *
  */

import scala.io.Source

object FunctionalTextAnalysis extends App {

  //the non-trivial words in the song name
  val nonTrivial = Array("LOVE", "ME", "IN", "CHIQUITITA")

  //reads in the top40 file and splits it into a tuple3
  val file = Source.fromFile("top40.txt").getLines.map(line => {
    val ln = line.split("'")
    val artist = ln(1)
    val songName = ln(3).split("\\s+")
    val numone = ln(4).charAt(2).toInt
    (artist, songName, numone)
  })

  //variable that contains the Set (a list of attributes that doesn't repeat) of artists and songs
  val lstArtist = file.foreach(record => record._1)
  val lstSongs = file.foreach(record => record._2)

  //the count method that counts how many of something there is
  //def counter()

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