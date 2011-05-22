package de.fuberlin.wiwiss.silk.impl.transformer

import de.fuberlin.wiwiss.silk.linkspec.input.Transformer
import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation

@StrategyAnnotation(id = "stem", label = "Stem", description = "Stems a string using the Porter Stemmer.")
class StemmerTransformer() extends Transformer
{
  override def evaluate(strings : Seq[String]) =
  {
    val stemmer = new PorterStemmer
    stemmer.stem(strings.toList.head)
  }
}
