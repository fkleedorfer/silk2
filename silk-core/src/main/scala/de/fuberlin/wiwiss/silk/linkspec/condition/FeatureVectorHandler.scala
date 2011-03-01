package de.fuberlin.wiwiss.silk.linkspec

import condition.FeatureInstance
import de.fuberlin.wiwiss.silk.util.strategy.{StrategyDefinition, Factory, Strategy}

/**
 * Trait that allows for handling a number of comparison results. This handler is designed
 * to collaborate with aggregations: Before the aggregation value is computed from the list 
 * of comparison results, registered handlers can act on the list, eg for Debugging.
 * 
 * @author Florian Kleedorfer
 *
 */
trait FeatureVectorHandler extends Strategy
{
  /**
   * Handles a vector that results from one or more comparisons.
   * @param comparisonVector
   */
  def handleFeatureVector(featureVector : Traversable[Option[FeatureInstance]])
}

object FeatureVectorHandler extends Factory[FeatureVectorHandler]

trait FeatureVectorEmitter
{
    var featureVectorHandlers: Traversable[FeatureVectorHandler] = List()

    def setFeatureVectorHandlers(handlers: Traversable[FeatureVectorHandler]) =
    {
        featureVectorHandlers = handlers
    }
    
    def emitFeatureVector(vector: Traversable[Option[FeatureInstance]]) =
    {
        if (featureVectorHandlers != null) {
            featureVectorHandlers.foreach(_.handleFeatureVector(vector))
        }
    }
    
}