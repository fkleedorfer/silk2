package de.fuberlin.wiwiss.silk.impl.metric

import de.fuberlin.wiwiss.silk.linkspec.condition.SimpleDistanceMeasure
import scala.math.max
import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation

@StrategyAnnotation(id = "levenshtein", label = "Normalized Levenshtein distance", description = "Normalized Levenshtein distance.")
case class LevenshteinMetric(minChar : Char = '0', maxChar : Char = 'z') extends SimpleDistanceMeasure
{
  private val q = 1

  private val levenshtein = new LevenshteinDistance(minChar, maxChar)

  override def evaluate(str1 : String, str2 : String, limit : Double) =
  {
    val scale = max(str1.length, str2.length)

    levenshtein.evaluate(str1, str2, limit * scale) / scale
  }

  override def index(str : String, limit : Double) : Set[Seq[Int]] =
  {
    levenshtein.index(str, limit * str.length)
  }

  override def blockCounts(threshold : Double) : Seq[Int] =
  {
    levenshtein.blockCounts(threshold)
  }
}
