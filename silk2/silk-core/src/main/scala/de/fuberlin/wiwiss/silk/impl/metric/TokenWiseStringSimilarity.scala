package de.fuberlin.wiwiss.silk.impl.metric

import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import java.util.regex.Pattern
import de.fuberlin.wiwiss.silk.linkspec.condition.{SimpleSimilarityMeasure, SimilarityMeasure}
import sun.font.TrueTypeFont
import collection.immutable.LinearSeq
import javax.management.remote.rmi._RMIConnection_Stub

/**
 * <p>
 * Similarity measure for strings. Strings are split into tokens and a similarity
 * measure is applied to compare all tokens with each other. The comparison results
 * are filtered such that no token appears in more than one comparison. The
 * individual token comparisons are aggregated by a technique similar to
 * the jaccard set similiarty in the following way:
 * <p>
 *
 * <p>
 * The sets to be compared are the token sets obtained from each string.
 * </p>
 *
 * <p>
 * The analogue figure of the set intersection size in the jaccard similarity is the
 *  sum over all similarities, i.e intersectionScore = Sum[over all matches](score)
 * The analogue figure of the set union size in the jaccard similarity is the sum of
 *  unmatched tokens in both token sets, plus the sum of the similarity score that was NOT attained
 *  for each matched token, i.e. unionScore = |unmatchedA| + |unmatchedB| + Sum[over all matches](score + 2*(1-score))
 * The final score is computed as intersectionScore / unionScore
 * </p>
 *
 * <p>
 * Tokens can be weighted individually (ideally using their IDF score in a corpus). The current
 * implementation only allows for defining stopwords which are weighted differently from normal tokens,
 * but the measure can easily be extended to use individual token weights.
 * </p>
 *
 * <p>
 * The token weights affect the score computation as follows:<br>
 * <ul>
 *  <li>Matched tokens:</li>
 *  <ul>
 *    <li>intersectionScore += token1_weight * token2_weight * match_score </li>
 *    <li>unionScore +=  token1_weight * token2_weight * match_score   +   (1 - match_score) * (token1_weight^2 + token2_weight^2) </li>
 *  </ul>
 *  <li>Unmatched tokens: unionScore += token_weight^2</li>
 * </ul>
 * </p>
 *
 * <p>
 * The parameter matchThreshold is used to disallow token matches below a certain threshold.
 * </p>
 *
 * <p>
 * Ordering of tokens in both input strings can also be taken into account. The parameter orderingImpact defines the
 * impact ordering has on the final score. If orderingImpact > 0.0, the positions of the matched tokens are compared
 * using kendall's tau, which yields 1 for identical ordering and 0 for reverse ordering.
 * The final score is computed as score * (1 - orderingImpact * (1 - tau)), which means that the maximum score for
 * input strings with tokens in exactly reverse order is 1 - orderingImpact
 * </p>
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
        nonStopwordWeight: Double = 0.1,
        matchThreshold: Double = 0.0,
        orderingImpact: Double = 0.0
        ) extends SimpleSimilarityMeasure
{
  require(stopwordWeight >= 0.0 && stopwordWeight <= 1.0, "stopwordWeight must be in [0,1]")
  require(nonStopwordWeight >= 0.0 && nonStopwordWeight <= 1.0, "nonStopwordWeight must be in [0,1]")
  require(matchThreshold >= 0.0 && matchThreshold <= 1.0, "matchThreshold must be in [0,1]")
  require(orderingImpact >= 0.0 && orderingImpact <= 1.0, "orderingImpact must be in [0,1]")
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
    // generate array of tokens
    val tokens1 = string1.split(splitRegex).map(x => if (ignoreCase) x.toLowerCase else x).toArray
    val tokens2 = string2.split(splitRegex).map(x => if (ignoreCase) x.toLowerCase else x).toArray
    // store weight for each token in array
    val weights1 = (for (token <- tokens1) yield getWeight(token)).toArray
    val weights2 = (for (token <- tokens2) yield getWeight(token)).toArray
    var debug = false
    //evaluate metric for all pairs of tokens and create triple (score, wordIndex1, wordIndex2)
    val scores = for (ind1 <- 0 to tokens1.size - 1; ind2 <- 0 to tokens2.size - 1; score = metric.evaluate(tokens1(ind1), tokens2(ind2), threshold) if score >= matchThreshold) yield {
      (score, ind1, ind2)
    }
    //now sort by score
    val sortedScores= scores.sortBy(t => -t._1)
    if (debug) println ("sorted scores: " + sortedScores.mkString(","))
    //
    val matchedTokens1 = new Array[Boolean](tokens1.size)
    val matchedTokens2 = new Array[Boolean](tokens2.size)
    var matchedCount1 = 0
    var matchedCount2 = 0
    var lastScore = 1.0

    //optimized version of the match calculation:
    // select best matches, only one match for each token
    val alignmentScores =
      for (triple <- sortedScores if
         matchedCount1 < tokens1.size &&
         matchedCount2 < tokens2.size &&
         lastScore > 0.0 &&
         matchedTokens1(triple._2) == false &&
         matchedTokens2(triple._3) == false)
      yield
      {
        lastScore = triple._1
        matchedTokens1(triple._2) = true
        matchedTokens2(triple._3) = true
        matchedCount1 += 1
        matchedCount2 += 1
        triple
      }

/////  original computation of alignmentScores, slightly slower with longer strings (~10-15% for 2 strings with about 25 words)
//    //now select only the highest score for each word
//    val alignmentScores = sortedScores.foldLeft[Seq[Triple[Double,Int,Int]]](Seq.empty)((triples, triple) => {
//      //check if we can add the current triple or not
//      if (triples.size == 0) {
//        Seq(triple)
//      } else if (triples.exists(x => x._2 == triple._2 || x._3 == triple._3)) {
//        //one of the words in the current comparison has already been selected, so we can't add it to the result
//        triples
//      } else {
//        //none of the words in the current comparison has been matched so far, add it if it has score > 0
//        if (triple._1 > 0.0) {
//          triple +: triples
//        } else {
//          triples
//        }
//      }
//    })

    if (debug) println("string1: " + string1 +", words1=" + tokens1.mkString(","))
    if (debug) println("string2: " + string2 +", words2=" + tokens2.mkString(","))
    if (debug) println("alignmentScores=" + alignmentScores.map(x => (x._1, tokens1(x._2), tokens2(x._3) )).mkString("\n"))

    //calculate score for each match: weight_of_word1 * weight_of_word2 * score, and sum
    //in the jaccard-like aggregation this is the 'intersection' of the two token sets
    var intersectionScore = 0.0
    //calculate score not reached for each match: weight_of_word1 * weight_of_word2 * (2 - score)[= 1+(1-score)], and sum
    //in the jaccard-like aggregation this is the 'union' of the two token sets, where they are matched
    var unionScoreForMatched = 0.0
    for (alignmentScore <- alignmentScores) {
      val weight1 = weights1(alignmentScore._2)
      val weight2 = weights2(alignmentScore._3)
      val tmpIntersectionScore = weight1 * weight2 * alignmentScore._1 //~jaccard intersection
      intersectionScore += tmpIntersectionScore
      unionScoreForMatched += tmpIntersectionScore + (math.pow(weight1,2) + math.pow(weight2,2)) * ( 1.0 - alignmentScore._1 ) //~ jaccard union wrt matches
    }
////// original calculation of intersectionScore and unionScoreForMatched
//    //calculate score for each match: weight_of_word1 * weight_of_word2 * score, and sum
//    //in the jaccard-like aggregation this is the 'intersection' of the two token sets
//    val intersectionScore = alignmentScores.foldLeft[Double](0)((sum,t) => sum + getWeight(words1(t._2)) * getWeight(words2(t._3)) * t._1) //~jaccard intersection
//    //now, calculate score not reached for each match: weight_of_word1 * weight_of_word2 * (2 - score)[= 1+(1-score)], and sum
//    //in the jaccard-like aggregation this is the 'union' of the two token sets, where they are matched
//    val unionScoreForMatched = alignmentScores.foldLeft[Double](0)((sum,t) => sum + getWeight(words1(t._2)) * getWeight(words2(t._3)) * t._1 + (math.pow(getWeight(words1(t._2)),2) + math.pow(getWeight(words2(t._3)),2)) * ( 1.0 - t._1)) //~ jaccard union wrt matches


    //now calculate a penalty score for the words that weren't matched
    //in the jaccard-like aggregation, this is the 'union' of the two token sets where they are not matched


    val unmatchedIndices1 = matchedTokens1.zipWithIndex.filter(!_._1).map(_._2)
    val unmatchedIndices2 = matchedTokens2.zipWithIndex.filter(!_._1).map(_._2)
    val unionScoreForUnmatched = unmatchedIndices1.map(x => math.pow(weights1(x),2)).sum + unmatchedIndices2.map(x => math.pow(weights2(x),2)).sum // ~jaccard union wrt unmatched words

    if (debug) println("matchedTokens1 = " + matchedTokens1.mkString(","))
    if (debug) println("unmatchedIndices1 = " + unmatchedIndices1.mkString(","))


///// original calculation of unionScoreForUnmatched
//    val unmatchedWords1 = words1.indices.diff(alignmentScores.map[Int,Seq[Int]](x => x._2)).map(words1(_))
//    if (debug) println("unmatched1: " + unmatchedWords1.mkString("\n"))
//    val unmatchedWords2 = words2.indices.diff(alignmentScores.map[Int,Seq[Int]](x => x._3)).map(words2(_))
//    if (debug) println("unmatched2: " + unmatchedWords2.mkString("\n"))
//val unionScoreForUnmatched = unmatchedWords1.map(x => math.pow(getWeight(x),2)).sum + unmatchedWords2.map(x => math.pow(getWeight(x),2)).sum // ~jaccard union wrt unmatched words

    if (debug) println("intersectionScore=" + intersectionScore)
    if (debug) println("unionScoreForMatched=" + unionScoreForMatched)
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
    if (orderingImpact > 0.0 && alignmentScores.size > 1) {
      val matchIndices1 = alignmentScores.map(_._2).zipWithIndex.sortBy(x => -x._1).map(_._2)
      val matchIndices2 = alignmentScores.map(_._3).zipWithIndex.sortBy(x => -x._1).map(_._2)
      if (debug) println("matchIndices1=" + matchIndices1.mkString(","))
      if (debug) println("matchIndices2=" + matchIndices2.mkString(","))
      val tau = kendallsTau(matchIndices1, matchIndices2)
      if (debug) println("tau=" + tau)
      val scoreWithOrdering = score * (1 - orderingImpact * ( 1 - tau))
      if (debug) println("scoreWithOrdering=" + scoreWithOrdering)
      scoreWithOrdering
    } else {
      score
    }
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

  /**
   * Vanilla implementation of kendall's tau with complexity O(n^2)
   */
  def kendallsTau(seq1: Seq[Int], seq2: Seq[Int]): Double =
  {
    require(seq1.size == seq2.size, "for calculating kendall's tau, the sequences must be of equal size")
    if (seq1.size == 1) return 1.0
    val arr1 = seq1.toArray
    val arr2 = seq2.toArray
    val numerator = (for (i <- 0 to arr1.size -1 ; j <- 0 to i-1) yield {
      if (math.signum(arr1(i) - arr1(j)) == math.signum(arr2(i) - arr2(j))){
        1.0
      } else {
        0.0
      }
    }).sum
    numerator / (0.5 * (arr1.size * (arr1.size -1)))
  }

}

