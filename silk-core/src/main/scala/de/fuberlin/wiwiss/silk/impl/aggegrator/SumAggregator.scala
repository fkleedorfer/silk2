package de.fuberlin.wiwiss.silk.impl.aggegrator

import de.fuberlin.wiwiss.silk.linkspec.condition.FlatIndexAggregator
import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation

@StrategyAnnotation(id = "sum", label = "Sum", description = "Sums all values.")
class SumAggregator() extends FlatIndexAggregator
{
  override def evaluate(values : Traversable[(Int, Double)]) =
  {
    if (values.isEmpty) None else Some(values.map(_._2).sum)
  }
}