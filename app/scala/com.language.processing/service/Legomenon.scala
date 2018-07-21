package com.language.processing.service

// Author: Saqib Ali

object Legomenon {
      
      //  hapax legomenon, dis legomenon, tris legomenon, and tetrakis legomenon implementation in Scala
      
      val stopWords = StopWords.words.toList

      def getLegomenon(signs: List[List[String]], repeated: Int = 1): List[(String, List[List[String]])] = {
            
      val punctuationSet = (")(,.?;!:").toSet

      (for( i <- 0 to signs.length-1) yield  signs(i)(2)
            .split(" ")
            .map(x => signs(i).patch(2, List(x.filterNot(punctuationSet.contains(_))), 0))
      )
      .toList
      .flatten
      .groupBy{case(List(chapter, verse, word, text, city)) => word.capitalize}
      .toList
      .filter{case(ngram, occurrences) => occurrences.length == repeated }
      .filterNot(e => stopWords.contains(e._1))

      
      
      
      }
}

  
    

