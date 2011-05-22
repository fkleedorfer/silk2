package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.linkspec.input.Input
import de.fuberlin.wiwiss.silk.config.Prefixes

/**
 * A comparison computes the similarity of two inputs.
 */


case class Comparison(required : Boolean, weight : Int, threshold: Double, inputs : SourceTargetPair[Input], metric : Metric, debugLabel: String = null) extends Operator
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
    var debugStr:String = null
    if (debugLabel != null){
      debugStr = ", set1=" + set1 + ", set2=" + set2 + ", inst1=" + instances.source.uri + ", inst2=" + instances.target.uri
    }


    if(!set1.isEmpty && !set2.isEmpty)
    {
      val similarities = for (str1 <- set1; str2 <- set2) yield metric.evaluate(str1, str2, threshold)
      val maxSim = similarities.max
      if (maxSim >= this.threshold ){
        if (debugLabel != null){
          logger.info(debugLabel + ": value=" + maxSim + debugStr)
        }
        Some(maxSim)
      } else {
        if (debugLabel != null){
          logger.info(debugLabel + ": value=" + maxSim +"(below threshold of " + threshold + ") " + debugStr)
        }
        None
      }
    }
    else
    {
      if (debugLabel != null){
        logger.info(debugLabel + ": value=[no comparison possible]" + debugStr)
      }
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

  override def toXML(implicit prefixes : Prefixes) = metric match
  {
    case Metric(id, params) =>
    {
      <Compare required={required.toString} weight={weight.toString} metric={id}>
        { inputs.source.toXML }
        { inputs.target.toXML }
        { params.map{case (name, value) => <Param name={name} value={value} />} }
      </Compare>
    }
  }
}
