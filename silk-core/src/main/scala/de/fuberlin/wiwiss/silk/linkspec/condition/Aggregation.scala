package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import de.fuberlin.wiwiss.silk.config.Prefixes

case class Aggregation(required : Boolean, weight : Int, threshold: Double, operators : Seq[Operator], aggregator : Aggregator) extends Operator
{
  /**
   * Computes the similarity between two instances.
   *
   * @param instances The instances to be compared.
   * @param threshold The similarity threshold.
   *
   * @return The similarity as a value between 0.0 and 1.0.
   *         Returns 0.0 if the similarity is lower than the threshold.
   *         None, if no similarity could be computed.
   */
  override def apply(instances : SourceTargetPair[Instance], threshold : Double) : Option[Double] =
  {
    val totalWeights = operators.map(_.weight).sum

    val weightedValues =
    {
      for(operator <- operators) yield
      {
        val value = operator(instances, aggregator.computeThreshold(threshold, operator.weight.toDouble / totalWeights))
        if(operator.required && value.isEmpty) return None

        (operator.weight, value.getOrElse(0.0))
      }
    }
    if (required && weightedValues.size == 0) return None
    var result = aggregator.evaluate(weightedValues)
    if (result.isEmpty) return None
    var actualValue=result.get
    if (actualValue < this.threshold){
      return None
    }
    result
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
    val totalWeights = operators.map(_.weight).sum

    val indexSets =
    {
      for(op <- operators) yield
      {
        val index = op.index(instance, aggregator.computeThreshold(threshold, op.weight.toDouble / totalWeights))
        val blockCounts = op.blockCounts

        if(op.required && index.isEmpty) return Set.empty;

        (index, blockCounts)
      }
    }

    if(indexSets.isEmpty)
    {
      Set.empty
    }
    else
    {
      val combined = indexSets.reduceLeft[(Set[Seq[Int]], Seq[Int])]
      {
        case ((indexSet1, blockCounts1), (indexSet2, blockCounts2)) =>
        {
          val combinedIndexSet = aggregator.combineIndexes(indexSet1, blockCounts1, indexSet2, blockCounts2)
          val combinedBlockCounts = aggregator.combineBlockCounts(blockCounts1, blockCounts2)

          (combinedIndexSet, combinedBlockCounts)
        }
      }

      combined._1
    }
  }

  /**
   * The number of blocks in each dimension of the index.
   */
  override val blockCounts : Seq[Int] =
  {
    operators.map(_.blockCounts)
             .foldLeft(Seq[Int]())((blockCounts1, blockCounts2) => aggregator.combineBlockCounts(blockCounts1, blockCounts2))
  }

  override def toXML(implicit prefixes : Prefixes) = aggregator match
  {
    case Aggregator(id, params) =>
    {
      <Aggregate required={required.toString} weight={weight.toString} type={id}>
        { operators.map(_.toXML) }
      </Aggregate>
    }
  }
}
