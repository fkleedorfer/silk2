package de.fuberlin.wiwiss.silk.impl.metric

import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FlatSpec

class TokenwiseStringSimilarityTest extends FlatSpec with ShouldMatchers
{
    val metric = new TokenwiseStringSimilarity(metricName = "levenshtein", stopwords="and or in on the a from", nonStopwordWeight = 0.1, stopwordWeight=0.001)

    "TokenwiseStringSimilarity" should "return distance 1 (several seditious scribes, several seditious scribes)" in
    {
        metric.evaluate("several seditious scribes", "several seditious scribes", 0.0) should equal (1)
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


}