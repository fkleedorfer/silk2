package de.fuberlin.wiwiss.silk.impl.comparisonvectorhandler

import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import java.util.logging.Logger
import java.io._
import de.fuberlin.wiwiss.silk.linkspec.condition.FeatureInstance
import util.Random
import de.fuberlin.wiwiss.silk.linkspec.{FeatureVectorHandler}

@StrategyAnnotation(id = "csv", label = "Csv", description = "output each comparison vector in csv format")
class CsvFeatureVectorHandler(file: String, classField: String, negativeExampleProbability: Double, positiveExampleProbability: Double, missingValue: String) extends FeatureVectorHandler {
    private val logger = Logger.getLogger(classOf[CsvFeatureVectorHandler].getName)
    private val outfile = new File(file)
    private var writer = new PrintWriter(new FileOutputStream(outfile))
    private val random = new Random(System.currentTimeMillis())
    private var initialized = false;
    private var classFieldIndex:Int = -1
    
    //TODO: this never gets called
    def closeWriter() = {
        writer.close();
    }

    private def initialize(featureVector: Traversable[Option[FeatureInstance]]) = {
      writer.println(featureVector.map{
        case Some(featureInstance) => featureInstance.featureName
        case _ => "no name"
      }.mkString(","))
      writer.flush()
      classFieldIndex = featureVector.toList.indexWhere
        {
          case None => false
          case Some(inst:FeatureInstance) => inst.featureName == this.classField
        }
      logger.info("class field index = " + classFieldIndex)
      initialized = true
    }

    override def handleFeatureVector(featureVector : Traversable[Option[FeatureInstance]]) = {
        if (!initialized){
          initialize(featureVector)
        }
        //System.out.println("handling feature vector:" + featureVector)
        //logger.info("writing comparison vector to csv file: " + comparisonVector);
        val randomValue = random.nextDouble()
        //logger.info("csv handling feature vector:" + featureVector)
        var threshold = 1.0
        if (isPositiveExample(featureVector))  {
          threshold = positiveExampleProbability
        } else {
          threshold = negativeExampleProbability
        }

        if (randomValue <= threshold) {
          def line = featureVector.map
              {
                case Some(feature) => makeFeatureInstanceString(feature)
                case _ => missingValue
              }.mkString(",")
          //def line = comparisonVector.map(v=>v._2).map(x => {if (x) '?' else x.toString()}).mkString(",")
          writer.println(line)
          writer.flush()
        }
    }

    def makeFeatureInstanceString(feature: FeatureInstance):String =
    {
       if (feature.value.isEmpty){
         missingValue
       } else {
         feature.dataType match {
          case "numeric" => feature.value.get.toDouble.toString
          case "nominal" => feature.value.get
          case "boolean" => feature.value.get.toBoolean.toString
          case "ordinal" => feature.value.get.toInt.toString
          case _ => "unknown type (" + feature.dataType + ")=" + feature.value
         }
       }
    }


    def isPositiveExample(featureVector: Traversable[Option[FeatureInstance]]): Boolean =
    {

      if (this.classFieldIndex == -1) {
        logger.info("cannot check for positive example: no classField index found")
        false
      } else {
        val classFieldValue = featureVector.toList(classFieldIndex)
        if (classFieldValue.isEmpty || classFieldValue.get.value.isEmpty){
//          logger.info("negative example: " + featureVector)
          false
        } else {
          if (classFieldValue.get.value.get == "true"){
//            logger.info("positive example: " + featureVector)
            true
          } else {
//            logger.info("negative example: " + featureVector)
            false
          }
        }
      }
    }

}

