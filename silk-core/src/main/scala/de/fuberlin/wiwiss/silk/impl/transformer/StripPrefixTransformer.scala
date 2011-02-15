package de.fuberlin.wiwiss.silk.impl.transformer

import de.fuberlin.wiwiss.silk.linkspec.input.Transformer
import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation

@StrategyAnnotation(id = "stripPrefix", label = "Strip prefix", description = "Strips a prefix of a string.")
class StripPrefixTransformer(prefix : String) extends Transformer
{
  override def evaluate(strings : Seq[String]) : String =
  {
    val word = strings.toList.head
    if (word.startsWith(prefix))
      return word.substring(prefix.size, word.size)
    else
      return word
  }
}