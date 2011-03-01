package de.fuberlin.wiwiss.silk.impl.classifier

import de.fuberlin.wiwiss.silk.linkspec.MultiIndexAggregator
import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import de.fuberlin.wiwiss.silk.linkspec.condition.{FeatureInstance, MultiIndexClassifierAggregator}
import util.Random

@StrategyAnnotation(id = "random", label = "Random", description = "Produces random classification results (just for testing purposes)")
class RandomClassifierAggregator() extends MultiIndexClassifierAggregator
{
  private val random = new Random(System.currentTimeMillis())

  override def classify(featureVector : Traversable[Option[FeatureInstance]]) =
  {
    if((featureVector.isEmpty || featureVector.exists(_.isEmpty)))
    {
      None
    }
    else
    {
      Some(random.nextInt(2).toDouble)
    }
  }
}