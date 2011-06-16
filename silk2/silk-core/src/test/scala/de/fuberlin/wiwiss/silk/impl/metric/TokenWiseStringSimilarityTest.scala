package de.fuberlin.wiwiss.silk.impl.metric

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

class TokenwiseStringSimilarityTest extends FlatSpec with ShouldMatchers
{
    val metric = new TokenwiseStringSimilarity(metricName = "levenshtein", stopwords="and or in on the a from thy mr mrs who", nonStopwordWeight = 0.1, stopwordWeight=0.001)

    "TokenwiseStringSimilarity" should "return distance 1 (several seditious scribes, several seditious scribes)" in
    {
        metric.evaluate("several seditious scribes", "several seditious scribes", 0.0) should equal (1)
    }

    "TokenwiseStringSimilarity" should "return distance 1 (several seditious scribes, scribes seditious several)" in
    {
        metric.evaluate("several seditious scribes", "scribes seditious several", 0.0) should equal (1)
    }


    "TokenwiseStringSimilarity" should "return distance 0.749 (several seditious scribes, several seditious scribes from caesarea)" in
    {
        metric.evaluate("several seditious scribes", "several seditious scribes from caesarea", 0.0) should be (0.749 plusOrMinus 0.001)
        metric.evaluate("several seditious scribes from caesarea", "several seditious scribes", 0.0) should be (0.749 plusOrMinus 0.001)
    }

    "TokenwiseStringSimilarity" should "return distance 0.90909 (several seditious scribes, several seditious scibes)" in
    {
        metric.evaluate("several seditious scibes", "several seditious scribes", 0.0) should be (0.909 plusOrMinus 0.001)
        metric.evaluate("several seditious scribes", "several seditious scibes", 0.0) should be (0.909 plusOrMinus 0.001)
    }

    "TokenwiseStringSimilarity" should "return distance 0.0 (,)" in
    {
        metric.evaluate("", "", 0.0) should equal (0.0)
    }

    "TokenwiseStringSimilarity" should "return distance 0.0 (Anything,)" in
    {
        metric.evaluate("Anything", "", 0.0) should equal (0.0)
        metric.evaluate("", "Anything", 0.0) should equal (0.0)
    }

    "TokenwiseStringSimilarity" should "return distance 0.999 (Hotel Hilton in Manhattan, hotel hilton manhattan)" in
    {
        metric.evaluate("Hotel Hilton in Manhattan", "hotel hilton manhattan", 0.0) should be (0.999 plusOrMinus 0.001)
    }

    "TokenwiseStringSimilarity" should "return different distances for similar terms if stopwords are involved" in
    {
        //both of these are stopwords - match score is high
        metric.evaluate("the", "thy", 0.0) should be (0.5 plusOrMinus 0.001)
        //one of these is a stopword - match score is low
        metric.evaluate("and", "any", 0.0) should be (0.0196 plusOrMinus 0.001)
        //none of these is a stopword - match score is high
        metric.evaluate("war", "was", 0.0) should be (0.5 plusOrMinus 0.001)
        //a match where the stopwords in both strings are matched - they don't contribute much to the result
        metric.evaluate("Mr Doe", "Mrs Dow", 0.0) should be (0.5 plusOrMinus 0.001)
        //a match where the stopwords in both strings are matched - they don't contribute much to the result
        metric.evaluate("Mr John Doe", "Mrs John Doe", 0.0) should be (1.0 plusOrMinus 0.0001)
        //identical match containing stopwords:
        metric.evaluate("Mr John Doe", "Mr John Doe", 0.0) should equal (1.0)
        //all-stopwords matches in comparison (try this with normal stopword processing!):
        metric.evaluate("the who", "the who", 0.0) should equal (1.0)
        metric.evaluate("the the", "the who", 0.0) should equal (0.5)
    }

    "TokenwiseStringSimilarity" should "return distance 0.5 (Hotel Hotel, Hotel)" in
    {
        //test if only one of two identical tokens is matched
        metric.evaluate("Hotel Hotel", "Hotel", 0.0) should be (0.5 plusOrMinus 0.001)
    }

    "TokenwiseStringSimilarity" should "return distance 0.5 (Hotel Hotel, Hotel) if token length is taken into account" in
    {
        val myMetric = new TokenwiseStringSimilarity(metricName = "levenshtein", stopwords="and or in on the a from thy mr mrs", nonStopwordWeight = 0.1, stopwordWeight=0.001, adjustByTokenLength = true)
        myMetric.evaluate("Hotel Hotel", "Hotel", 0.0) should be (0.5 plusOrMinus 0.001)
    }

    "TokenwiseStringSimilarity" should "return distance 0.333 (Hotel California, Hotel) if token length is taken into account" in
    {
        val myMetric = new TokenwiseStringSimilarity(metricName = "levenshtein", stopwords="and or in on the a from thy mr mrs", nonStopwordWeight = 0.1, stopwordWeight=0.001, adjustByTokenLength = true)
        myMetric.evaluate("Hotel California", "Hotel", 0.0) should be (0.333 plusOrMinus 0.001)
    }

    "TokenwiseStringSimilarity" should "return distance 0.5 (several seditious scribes, scribes seditious several) with orderingImpact of 0.5" in
    {
        val myMetric = new TokenwiseStringSimilarity(metricName = "levenshtein", stopwords="and or in on the a from thy mr mrs", nonStopwordWeight = 0.1, stopwordWeight=0.001, orderingImpact = 0.5)
        myMetric.evaluate("several seditious scribes", "scribes seditious several", 0.0) should equal (0.5)
    }

    "TokenwiseStringSimilarity" should "return different distances for when matchThreshold is used (several seditious scribes, several sedated scribes)" in
    {
        val myMetric = new TokenwiseStringSimilarity(metricName = "levenshtein", stopwords="and or in on the a from thy mr mrs", nonStopwordWeight = 0.1, stopwordWeight=0.001, matchThreshold = 0.85)
        metric.evaluate("several seditious scribes", "several sedated scribes", 0.0) should be (0.687 plusOrMinus 0.001)
        myMetric.evaluate("several seditious scribes", "several sedated scribes", 0.0) should be (0.5 plusOrMinus 0.001)
    }

    "TokenwiseStringSimilarity" should "return 1.0 in (Sirenia + Niobeth, ould Sirenia and for Niobeth) with special settings" in
    {
        val myMetric = new TokenwiseStringSimilarity(metricName = "levenshtein", stopwords="and for ould", nonStopwordWeight = 1.0, stopwordWeight=0.0)
        myMetric.evaluate("Sirenia + Niobeth", "ould Sirenia and for Niobeth", 0.0) should equal(1.0)
    }


////// test for metric timing
//    "TokenwiseStringSimilarity" should "return 1.0 in (The expansion of chains like Abercrombie and Gap to Europe is based on a major shift in how young Europeans think about American fashion. , The expansion of chains like Gap and Abercrombie to Europe is fueled by a major shift in how young Europeans think about American fashion. ) with special settings" in
//    {
//        val myMetric = new TokenwiseStringSimilarity(metricName = "levenshtein", stopwords="and or in on the a from thy mr mrs",nonStopwordWeight = 0.1, stopwordWeight=0.001, orderingImpact = 0.2)
//        var time = System.nanoTime();
//        for (i <- 1 to 1000) {
//          myMetric.evaluate("The expansion of chains like Abercrombie and Gap to Europe is based on a major shift in how young Europeans think about American fashion.", "The expansion of chains like Gap and Abercrombie to Europe is fueled by a major shift in how young Europeans think about American fashion.", 0.0)
//        }
//        println("comparison of long strings took " +  ((System.nanoTime() - time).toDouble / 1000000.0) + " + millis")
//
//        time = System.nanoTime();
//        for (i <- 1 to 1000) {
//          myMetric.evaluate("Sirenia + Niobeth", "ould Sirenia and for Niobeth", 0.0)
//        }
//        println("comparison of short strings took " +  ((System.nanoTime() - time).toDouble / 1000000.0) + " + millis")
//
//    }
}