package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.linkspec._
import de.fuberlin.wiwiss.silk.config.Prefixes
import collection.mutable.{HashMap, SynchronizedMap, Map}

case class ClassifierAggregation(required:Boolean, weight: Int, threshold:Double, features: Traversable[Feature], classifier: ClassifierAggregator) extends Operator with FeatureVectorEmitter
{
  val logger = Logger.getLogger(classOf[ClassifierAggregation].getName)
  private val applyTime:Map[String,Long] = new HashMap[String, Long]() with SynchronizedMap[String, Long]
  private val indexTime:Map[String,Long] = new HashMap[String, Long]() with SynchronizedMap[String, Long]
  private var applyCount = 0
  private var indexCount = 0



  override def apply(instances: SourceTargetPair[Instance], threshold: Double): Option[Double] =
  {
//    applyCount += 1
//    if (applyCount > 0 && applyCount % 1000000 == 0){
//      printTimingInfo("comparison", applyCount, applyTime)
//    }
    val featureVector = {
      for (feature <- features) yield {
        //feature(instances, aggregator.computeThreshold(threshold, feature.weight.toDouble / totalWeights))
//        val start = System.nanoTime
        val result = feature.apply(instances,threshold)
//        addTimingInfo(applyTime, feature.featureName, System.nanoTime - start)
        result
      }
    }
    val result = classifier.classify(featureVector)
    if (result.isDefined) {
      emitFeatureVector(instances, featureVector, result, threshold)
    }
    if (! result.isEmpty && result.get >= this.threshold){
      result
    } else {
      None
    }
  }

  override def index(instance: Instance, threshold: Double): Set[Seq[Int]] =
  {
//    indexCount += 1
//    if (indexCount > 0 && indexCount % 5000 == 0){
//      printTimingInfo("indexing", indexCount, indexTime)
//    }
    val totalWeights = features.size

    val indexSets = {
      for (feature <- features) yield {
//        val start = System.nanoTime
        val index = feature.index(instance, classifier.computeThreshold(threshold, 1.0 / totalWeights))
//        addTimingInfo(indexTime, feature.featureName, System.nanoTime - start)
        val blockCounts = feature.blockCounts

        if (feature.required && index.isEmpty) return Set.empty;

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

  private def addTimingInfo(map: Map[String,Long], key:String, millis:Long) =
  {
    if (map.contains(key)) {
      map.put(key, map.get(key).get + millis)
    } else {
      map.put(key, millis)
    }
  }

  private def printTimingInfo(timingKind:String, count:Int, millis:Map[String, Long])
  {
    logger.info(timingKind + " stats: " + count + " calls performed. Time spent per feature: \n " +
      (for (key <- millis.keys) yield {
        "feature '" + key + "': " + millis.get(key).get / 1000000 + " millis"
      }).mkString("\n ")
    )
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