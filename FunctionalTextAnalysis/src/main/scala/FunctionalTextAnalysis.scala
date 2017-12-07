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
  val nonTrivial = List("LOVE", "ME")
  val artists = List("Elvis")

  //reads in the top40 file and splits it into a tuple3
  val file = Source.fromFile("top40.txt").getLines.map(line => {
    val ln = line.split("'").map(_.trim())
    val artist = ln(1)
    val songName = ln(3).split("\\s+")
    val numone = ln(4).charAt(2).toInt //prints out 32 for some reason, can't figure out why though

    /* print statments to check my if I'm reading from the file correctly
    songName.foreach(song=> print(song + " "))
    println("\n")
    println(artist + " " + numone)
    */

    //counts how many words is in the song name which can be used
    val songNum = songName.count(song => true)
    //println(songNum) //prints songNum

    //checks if the words in the song matches with the nont

    (artist, songName, numone)
  })


  //variable that contains the Set (a list of attributes that doesn't repeat) of artists and songs
  val setArtist = file.map(record => record._1).toSet
  val lstSongs = file.map(record => record._2)

  //counts how many songs that are in the nonTrivial list
  val numSongs = lstSongs.count(song=> if(song.contains(nonTrivial)) true else false)
  //println(numSongs)

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