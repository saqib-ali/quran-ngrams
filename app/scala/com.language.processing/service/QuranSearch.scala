package com.language.processing.service

object QuranSearch{
  def generateSearchResults(signs: List[List[String]], ngram: String): List[List[String]] = {
    signs.filter(_(2).matches("(?i:.*" + ngram + ".*)"))
  }
}