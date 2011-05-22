package de.fuberlin.wiwiss.silk.impl.transformer

import de.fuberlin.wiwiss.silk.linkspec.input.Transformer
import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation

@StrategyAnnotation(id = "replace", label = "Replace", description = "Replace all occurrences of a string \"search\" with \"replace\" in a string.")
class ReplaceTransformer(search : String, replace : String) extends Transformer
{
  override def evaluate(strings : Seq[String]) =
  {
    strings.toList.head.replace(search, replace)
  }
}
