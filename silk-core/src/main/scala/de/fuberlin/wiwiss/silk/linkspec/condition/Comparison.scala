package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.linkspec.input.Input

/**
 * A comparison computes the similarity of two inputs.
 */


case class Comparison(required : Boolean, weight : Int, threshold: Double, inputs : SourceTargetPair[Input], metric : Metric) extends Operator
{
  private val logger = Logger.getLogger(classOf[Comparison].getName)
  /**
   * Computes the similarity between two instances.
   *
   * @param instances The instances to be compared.
   * @param threshold The similarity threshold.
   *
   * @return The similarity as a value between 0.0 and 1.0. Returns 0.0 if the similarity is lower than the threshold.
   */

  override def apply(instances : SourceTargetPair[Instance], threshold : Double) : Option[Double] =
  {
    val set1 = inputs.source(instances)
    val set2 = inputs.target(instances)
    

    if(!set1.isEmpty && !set2.isEmpty)
    {
      val similarities = for (str1 <- set1; str2 <- set2) yield metric.evaluate(str1, str2, threshold)
      val maxSim = similarities.max
      if (maxSim >= this.threshold ){
        Some(maxSim)
      } else {
        None
      }
    }
    else
    {
      None
    }
  }

  /**
   * Indexes an instance.
   *
   * @param instance The instance to be indexed
   * @param threshold The similarity threshold.
   *
   * @return A set of (multidimensional) indexes. Instances within the threshold will always get the same index.
   */
  override def index(instance : Instance, threshold : Double) : Set[Seq[Int]] =
  {
    
    val values = inputs.source(SourceTargetPair(instance, instance)) ++ inputs.target(SourceTargetPair(instance, instance))
    values.flatMap(value => metric.index(value, threshold)).toSet
  }

  /**
   * The number of blocks in each dimension of the index.
   */
  override val blockCounts = metric.blockCounts
}
