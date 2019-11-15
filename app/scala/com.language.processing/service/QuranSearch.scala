package com.language.processing.service

import play.api.Logger
import java.util.regex.Pattern 



object QuranSearch{
  def generateSearchResults(signs: List[List[String]], ngram: String): List[List[String]] = {
    val infoLogger = Logger("info")
    infoLogger.info("Search string: " + ngram)
    println("Search string: " + ngram)


    signs.filter(_(2).matches("(?i:.*" + Pattern.quote(ngram) + ".*)"))
  }
}