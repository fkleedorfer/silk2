package de.fuberlin.wiwiss.silk.impl.metric

import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import java.util.regex.Pattern
import de.fuberlin.wiwiss.silk.linkspec.condition.{SimpleSimilarityMeasure, SimilarityMeasure}
import sun.font.TrueTypeFont

/**
 * Similarity measure for strings. Strings are split into tokens and a similarity
 * measure is applied to compare all tokens with each other. The comparison results
 * are filtered such that no token appears in more than one comparison. The
 * individual token comparisons are aggregated by a technique similar to
 * the jaccard set similiarty in the following way:
 *
 * The sets to be compared are the token sets obtained from each string.
 *
 * The analogue figure of the set intersection size in the jaccard similarity is the
 *  sum over all similarities, i.e intersectionScore = Sum[over all matches](score)
 * The analogue figure of the set union size in the jaccard similarity is the sum of
 *  unmatched tokens in both token sets, plus the sum of the similarity score that was NOT attained
 *  for each matched token, i.e. unionScore = |unmatchedA| + |unmatchedB| + Sum[over all matches](2*(1-score))
 * The final score is computed as intersectionScore / unionScore
 *
 * Tokens can be weighted individually (ideally using their IDF score in a corpus). The current
 * implementation only allows for defining stopwords which are weighted differently from normal tokens,
 * but the measure can easily be extended to use individual token weights.
 *
 * The weigths affects the score computation as follows:
 *
 * The similarity score of matched tokens is multiplied by the product of the token weights.
 *
 * The score calculated for unmatched tokens is the square of their weight.
 *
 * @author Florian Kleedorfer, Research Studios Austria
 */
@StrategyAnnotation(id = "tokenwiseSimilarity", label = "Token-wise Similarity", description = "Token-wise string similarity using the specified metric")
case class TokenwiseStringSimilarity(
        ignoreCase: Boolean = true,
        metricName: String = "levenshtein",
        splitRegex: String =  "[\\s\\d\\p{Punct}]+",
        stopwords: String = "",
        stopwordWeight: Double = 0.01,
        nonStopwordWeight: Double = 0.1
        ) extends SimpleSimilarityMeasure
{
  private val splitPattern = Pattern.compile(splitRegex)

  private val metric = metricName match {
    case "levenshtein" => new LevenshteinMetric()
    case "jaro" => new JaroDistanceMetric()
    case "jaroWinkler" => new JaroWinklerSimilarity()
    case _ => throw new IllegalArgumentException("unknown value '" + metricName +"' for parameter 'metricName', must be one of ['levenshtein', 'jaro', 'jaroWinkler')");
  }

  private val stopwordsSet = stopwords.split("[,\\s]+").map(x => if (ignoreCase) x.toLowerCase else x).toSet

  /**
   * Calculates a jaccard-like aggregation of string similarities of tokens. Order of tokens is not taken into account.
   */
  override def evaluate(string1: String, string2: String, threshold : Double) : Double =
  {
    val words1 = string1.split(splitRegex).map(x => if (ignoreCase) x.toLowerCase else x).toArray
    val words2 = string2.split(splitRegex).map(x => if (ignoreCase) x.toLowerCase else x).toArray

    var debug = true
    //evaluate metric for all pairs of words and create triple (score, wordIndex1, wordIndex2)
    val scores = for (ind1 <- 0 to words1.size - 1; ind2 <- 0 to words2.size - 1) yield {
      (metric.evaluate(words1(ind1), words2(ind2), threshold), ind1, ind2)
    }
    //now sort by score
    val sortedScores= scores.sortBy(t => -t._1)

    //now select only the highest score for each word
    val alignmentScores = sortedScores.foldLeft[Seq[Triple[Double,Int,Int]]](Seq.empty)((triples, triple) => {
      //check if we can add the current triple or not
      if (triples.size == 0) {
        Seq(triple)
      } else if (triples.exists(x => x._2 == triple._2 || x._3 == triple._3)) {
        //one of the words in the current comparison has already been selected, so we can't add it to the result
        triples
      } else {
        //none of the words in the current comparison has been matched so far, add it
        triple +: triples
      }
    })

    if (debug) println("string1: " + string1 +", words1=" + words1.mkString(","))
    if (debug) println("string2: " + string2 +", words2=" + words2.mkString(","))
    if (debug) println("alignmentScores=" + alignmentScores.map(x => (x._1, words1(x._2), words2(x._3) )).mkString("\n"))
    //calculate score for each match: weight_of_word1 * weight_of_word2 * score, and sum
    //in the jaccard-like aggregation this is the 'intersection' of the two token sets
    val intersectionScore = alignmentScores.foldLeft[Double](0)((sum,t) => sum + getWeight(words1(t._2)) * getWeight(words2(t._3)) * t._1) //~jaccard intersection
    //now, calculate score not reached for each match: weight_of_word1 * weight_of_word2 * (2 - score)[= 1+(1-score)], and sum
    //in the jaccard-like aggregation this is the 'union' of the two token sets, where they are matched
    val unionScoreForMatched = alignmentScores.foldLeft[Double](0)((sum,t) => sum + getWeight(words1(t._2)) * getWeight(words2(t._3)) * t._1 + (math.pow(getWeight(words1(t._2)),2) + math.pow(getWeight(words2(t._3)),2)) * ( 1.0 - t._1)) //~ jaccard union wrt matches


    //now calculate a penalty score for the words that weren't matched
    //in the jaccard-like aggregation, this is the 'union' of the two token sets where they are not matched
    val unmatchedWords1 = words1.indices.diff(alignmentScores.map[Int,Seq[Int]](x => x._2)).map(words1(_))
    if (debug) println("unmatched1: " + unmatchedWords1.mkString("\n"))
    val unmatchedWords2 = words2.indices.diff(alignmentScores.map[Int,Seq[Int]](x => x._3)).map(words2(_))
    if (debug) println("unmatched2: " + unmatchedWords2.mkString("\n"))
    if (debug) println("intersectionScore=" + intersectionScore)
    if (debug) println("unionScoreForMatched=" + unionScoreForMatched)

    val unionScoreForUnmatched = unmatchedWords1.map(x => math.pow(getWeight(x),2)).sum + unmatchedWords2.map(x => math.pow(getWeight(x),2)).sum // ~jaccard union wrt unmatched words
    if (debug) println("unionScoreForUnmatched=" + unionScoreForUnmatched)
    val unionScore = unionScoreForUnmatched + unionScoreForMatched

    if (debug) println("unionScore=" + unionScore)
    if (debug) println(alignmentScores.mkString("\n"))
    val score = if (unionScore == 0.0){
      1.0
    } else {
      intersectionScore / unionScore
    }
    if (debug) println("score=" + score)
    score
  }


  /**
   * Returns the weight associated with a token.
   * TODO: if there were a way to get useful IDF values for all tokens that can be expected,
   * this method should return the respective IDF value.
   */
  def getWeight(token: String): Double =
  {
    if (stopwordsSet(token)){
      stopwordWeight
    } else {
      nonStopwordWeight
    }
  }
}

