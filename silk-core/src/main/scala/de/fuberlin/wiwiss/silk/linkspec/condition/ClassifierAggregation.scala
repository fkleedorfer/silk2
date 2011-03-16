package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.linkspec._
import de.fuberlin.wiwiss.silk.config.Prefixes

case class ClassifierAggregation(required:Boolean, weight: Int, threshold:Double, features: Traversable[Feature], classifier: ClassifierAggregator) extends Operator with FeatureVectorEmitter
{
  override def apply(instances: SourceTargetPair[Instance], threshold: Double): Option[Double] =
  {
    val logger = Logger.getLogger(classOf[ClassifierAggregation].getName)

    val featureVector = {
      for (feature <- features) yield {
        //feature(instances, aggregator.computeThreshold(threshold, feature.weight.toDouble / totalWeights))
        feature.apply(instances,threshold)
      }
    }
    val result = classifier.classify(featureVector)
    if (result.isDefined) {
      //logger.info("comparisonVector.size=" + comparisonVector.size + ", operators.size=" + operators.size)
      emitFeatureVector(featureVector)
    }
    if (! result.isEmpty && result.get >= this.threshold){
      result
    } else {
      None
    }
  }

  override def index(instance: Instance, threshold: Double): Set[Seq[Int]] =
  {
    val totalWeights = features.size

    val indexSets = {
      for (op <- features) yield {
        val index = op.index(instance, classifier.computeThreshold(threshold, 1.0 / totalWeights))
        val blockCounts = op.blockCounts

        if (op.required && index.isEmpty) return Set.empty;

        (index, blockCounts)
      }
    }

    if (indexSets.isEmpty) {
      Set.empty
    }
    else {
      val combined = indexSets.reduceLeft[(Set[Seq[Int]], Seq[Int])]
          {
            case ((indexSet1, blockCounts1), (indexSet2, blockCounts2)) => {
              val combinedIndexSet = classifier.combineIndexes(indexSet1, blockCounts1, indexSet2, blockCounts2)
              val combinedBlockCounts = classifier.combineBlockCounts(blockCounts1, blockCounts2)

              (combinedIndexSet, combinedBlockCounts)
            }
          }

      combined._1
    }
  }

  override val blockCounts: Seq[Int] = {
    features.map(_.blockCounts)
      .foldLeft(Seq[Int]())((blockCounts1, blockCounts2) => classifier.combineBlockCounts(blockCounts1, blockCounts2))
  }

  override def toXML(implicit prefixes : Prefixes) =
  {
      <Classify note="xml serialization not implemented yet">
      </Classify>
  }
}