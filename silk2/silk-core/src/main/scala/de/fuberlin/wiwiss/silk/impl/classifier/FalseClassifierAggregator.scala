package de.fuberlin.wiwiss.silk.impl.classifier

import de.fuberlin.wiwiss.silk.linkspec.condition.MultiIndexAggregator
import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import de.fuberlin.wiwiss.silk.linkspec.condition.{FeatureInstance, MultiIndexClassifierAggregator}
import util.Random

@StrategyAnnotation(id = "alwaysFalse", label = "False", description = "Always produces score 0.0")
class FalseClassifierAggregator() extends MultiIndexClassifierAggregator
{

  override def classify(featureVector : Traversable[Option[FeatureInstance]]) =
  {
    if((featureVector.isEmpty || featureVector.exists(_.isEmpty)))
    {
      None
    }
    else
    {
      Some(0.0)
    }
  }
}