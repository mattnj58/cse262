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
  //the trivial words in the song name
  val trivial = List("I", "AND", "TO", "A", "IS", "FOR", "IN", "THE", "LOVE")

  //reads in the top40 file and splits it into a tuple3
  val file = Source.fromFile("top40.txt").getLines.map(line => {
    val ln = line.split("'").map(_.trim())
    val artist = ln(1)
    val songName = ln(3).split("\\s+").toList
    val numone = ln(4).charAt(2).toInt //prints out 32 for some reason, can't figure out why though

    /* print statments to check my if I'm reading from the file correctly
    songName.foreach(song=> print(song + " "))
    println("\n")
    println(artist + " " + numone)
    */

    //counts how many words is in the song name which can be used
    val songNum = songName.count(song => true)
    //println("The # of words in the song title is: " + songNum) //prints songNum

    (artist, songName, numone)
  })


  //variable that contains the Set (a list of attributes that doesn't repeat) of artists and songs
  //val setArtist = file.count(file.map(record => record._1).toSet)
 // print("The number of different artists are: " + setArtist)
  val lstSongs = file.map(record => record._2).toList


  //removes the trivial words from the list of songs
  def removeTriv(lstSong: List[List[String]], trivial: List[String]): List[String]={
    val nonTrivSongs = lstSong.map(song => song.dropWhile(word => word equals trivial)).flatten
    nonTrivSongs
  }

  //calls the method that removes the trivial words
  val nonTrivSongs = removeTriv(lstSongs, trivial)
  //nonTrivSongs.foreach(println)
  //println(nonTrivSongs.length)

  //counts how many times nontrivial words show up in the list of song names
  /*def mostCommonWords(nonTrivSongs: List[String]): List[Int] = {
    val counter = nonTrivSongs.map(word => nonTrivSongs.count( _== word))
    counter
  }
  */

  //the list of number of times a word shows up
  //val occurrences = mostCommonWords(nonTrivSongs)

  //tuple that shows how many times a word showed up
  val combo = nonTrivSongs.distinct.map(word => (word, nonTrivSongs.count(_==word)))
  //combo.foreach(com => println(com._1 + " " + com._2))

  //find the top three repeated words
  val orderedComboMeal = combo.sortWith(_._2>_._2)
  //orderedComboMeal.foreach(meal => println(meal._1 + " " + meal._2))

  //prints the top 5 repeated words
  println("The most used word is: " + orderedComboMeal(0))
  println("The second most used word is: " + orderedComboMeal(1))
  println("The third most used word is: " + orderedComboMeal(2))
  println("The fourth most used word is: " + orderedComboMeal(3))
  println("The fifth most used word is: " + orderedComboMeal(4))


  //method that calculates the probability given a certain word that's used
  def probCalc(orderedComboMeal: List[(String,Int)]): Unit = {
    
  }

  /*
  //a multiplication function
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
  */
}