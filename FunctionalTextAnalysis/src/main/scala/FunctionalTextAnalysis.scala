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
  val debug = true
  //the non-trivial words in the song name
  val nonTrivial = List("LOVE", "ME", "IN", "CHIQUITITA")
  val artists = List("Elvis")

  //reads in the top40 file and splits it into a tuple3
  val file = Source.fromFile("top40.txt").getLines.map(line => {
    val ln = line.split("'").map(_.trim())
    val artist = ln(1)
    val songName = ln(3).split("\\s+")
    val numone = (ln(4).charAt(0)).toInt
    println(artist + " " + numone)
    (artist, songName, numone)
  })



  //variable that contains the Set (a list of attributes that doesn't repeat) of artists and songs
  val setArtist = file.map(record => record._1).toSet
  val numArt = setArtist.size
  val lstSongs = file.map(record => record._2)
  val numSongs = lstSongs.length

  //counts how many words that are in the nonTrivial list
  val counter = nonTrivial.foreach(nTriv => lstSongs.foreach( _==nTriv))



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