package com.language.processing.service

import scala.collection.mutable.ListBuffer


import scala.concurrent.future
import scala.concurrent.{Await, Future}
import scala.util.{Success, Failure}
//import scala.collection.parallel.immutable.ParVector
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

// Author: Saqib Ali

object NGram {
  
    def generateNGram(signs: List[String], numOfWords: Int): List[(String, Int)] = { // Scala N-gram secret sauce 
    (for( i <- 0 to signs.length-1) yield  signs(i)
      .replaceAll("([\\p{P}&&[^()]]+\\s*)+$", "")
      .replaceAll("([\\p{P}&&[^()]]+\\s*)+$", "")
      .split(" ")
      .sliding(numOfWords)
      .filter(_.size==numOfWords)
      .map(_.mkString(" "))
      //.map(_.replaceAll("[\\p{P}\\s]+$", ""))
      //.map(_.replaceAll("[\\p{P}\\s]+$", ""))


    )
       .flatten
       .groupBy(x => x)   
       .toList   
       .map{case(ngram, occurrences) => (ngram, occurrences.length)}
       .filter{case(ngram, occurrences) => occurrences > 1}
  }

  def time[R](block: => R): R = { //Profile methods / code in Scala. Useful in timining the execution of scala code source: http://stackoverflow.com/q/9160001/420558
    val t0 = System.nanoTime()
    val result = block    
    val t1 = System.nanoTime()
    println("Elapsed time: " + (t1 - t0)/1000000000 + "s")
    result
}
  
  
  def generateNGramFuture(signs: List[String], numOfWords: Int): Future[List[(String, Int)]] = Future{ // Scala N-gram secret sauce
    (for( i <- 0 to signs.length-1) yield  signs(i)
      .replaceAll("([\\p{P}&&[^()]]+\\s*)+$", "")
      .replaceAll("([\\p{P}&&[^()]]+\\s*)+$", "")
      .split(" ")
      .sliding(numOfWords)
      .filter(_.size==numOfWords)
      .map(_.mkString(" "))
      //.map(_.replaceAll("([\\p{P}&&[^()]]+\\s*)+$", ""))
      //.map(_.replaceAll("([\\p{P}&&[^()]]+\\s*)+$", ""))
      )
      .flatten
      .groupBy(x => x)
      .toList
      .map{case(ngram, occurrences) => (ngram, occurrences.length)}
      .filter{case(ngram, occurrences) => occurrences > 1}
  }
  
  def longestNGram(signs: List[String]): List[(String, Int)] = {
      time{
      //val all = ((6 to 24).foldRight(List[(String, Int)]())((i, l) => l ::: generateNGram(signs, i))).sortWith(_._1.length > _._1.length)
      //val all = ((6 to 24).map(i => generateNGram(signs, i)).reduce(_ ::: _)).sortWith(_._1.length > _._1.length)
      val ngramfutures = (15 to 30).map(i => generateNGramFuture(signs, i)) // Calculate Ngrams using Scala Futures for paralllelization
      val fut = Future.reduceLeft(ngramfutures)(_ ::: _) // Reduce the Futures
      val allresult = Await.result(fut, 20 seconds)    
      val all = allresult.sortWith(_._1.length > _._1.length)
      all.map(calc(_, all))
      .distinct
      .sortBy(_._2)
      .reverse
      }
  }
     
  def sleep(time: Long) { Thread.sleep(time) }
   
  
  def calc(init: (String, Int), l: List[(String, Int)]): (String, Int) = {
    if (l.isEmpty) init
    else if (l.head._1.contains(init._1)) l.head
    else calc(init, l.tail)
  }
  
  

}