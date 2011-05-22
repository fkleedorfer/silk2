package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.linkspec.input.Input
import de.fuberlin.wiwiss.silk.config.Prefixes

/**
 * A generator for random values. Random number generation and blocking are delegated to the specified RandomGenerator.
 */


case class RandomOperator(required: Boolean, weight : Int, threshold: Double, randomGenerator : RandomGenerator) extends Operator
{
  private val logger = Logger.getLogger(classOf[RandomOperator].getName)
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
    val rnd = randomGenerator.nextValue();
    if (rnd <= threshold){
      if (required){
        //System.out.println("RandomOperator: None, generated random:" + rnd + ", threshold=" + threshold  )
        None
      } else {
        //System.out.println("RandomOperator: Some(0.0), generated random:" + rnd + ", threshold=" + threshold  )
        Some(0.0)
      }
    }
    //System.out.println("RandomOperator: Some(" + rnd + "), generated random:" + rnd + ", threshold=" + threshold  )
    Some(rnd)
  }

  /**
   * Indexes an instance.
   *
   * @param instance The instance to be indexed
   * @param threshold The similarity threshold.
   *
   * @return A set of (multidimensional) indexes. Instances within the threshold will always get the same index.
   */
  override def index(instance : Instance, threshold : Double) : Set[Seq[Int]] = randomGenerator.index(instance.uri)

  /**
   * The number of blocks in each dimension of the index.
   */
  override val blockCounts = randomGenerator.blockCounts

  override def toXML(implicit prefixes : Prefixes) = randomGenerator match
  {
    case RandomGenerator(id, params) =>
    {
      <Random required="required" weight={weight.toString} randomGenerator={id}>
        { params.map{case (name, value) => <Param name={name} value={value} />} }
      </Random>
    }
  }
}