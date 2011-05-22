package de.fuberlin.wiwiss.silk.linkspec

import condition.FeatureInstance
import de.fuberlin.wiwiss.silk.util.strategy.{StrategyDefinition, Factory, Strategy}
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import de.fuberlin.wiwiss.silk.output.OutputRow
import de.fuberlin.wiwiss.silk.instance.Instance


trait FeatureVectorOutput extends Strategy
{
  def initialize(header: Seq[String])
  def apply(fields: Seq[String])
}

object FeatureVectorOutput extends Factory[FeatureVectorOutput]

/**
 * Allows for handling a number of comparison results. This handler is designed
 * to collaborate with aggregations: Before the aggregation value is computed from the list 
 * of comparison results, registered handlers can act on the list, eg for Debugging.
 * 
 * @author Florian Kleedorfer
 *
 */
class FeatureVectorHandler(val outputRows: Seq[OutputRow], output: FeatureVectorOutput, ignoreThreshold: Boolean=false)
{

  /**
   * Handles a vector that results from one or more comparisons.
   * @param comparisonVector
   */
  def handleFeatureVector(instances: SourceTargetPair[Instance], featureVector : Traversable[Option[FeatureInstance]], classificationResult: Option[Double], threshold: Double) =
  {
    if (this.ignoreThreshold || classificationResult.isDefined && classificationResult.get >= threshold){
      for (outputRow <- this.outputRows)
      {
        output(outputRow(instances,featureVector,classificationResult))
      }
    }
  }
}


trait FeatureVectorEmitter
{
    var featureVectorHandlers: Traversable[FeatureVectorHandler] = List()

    def setFeatureVectorHandlers(handlers: Traversable[FeatureVectorHandler]) =
    {
        featureVectorHandlers = handlers
    }
    
    def emitFeatureVector(instances: SourceTargetPair[Instance], vector: Traversable[Option[FeatureInstance]], classificationResult: Option[Double], threshold: Double) =
    {
        if (featureVectorHandlers != null) {
            featureVectorHandlers.foreach(_.handleFeatureVector(instances, vector, classificationResult, threshold))
        }
    }
    
}

