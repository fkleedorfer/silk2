package de.fuberlin.wiwiss.silk.impl.classifier

import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import weka.classifiers.Classifier
import java.io.{File, ObjectInputStream, FileInputStream}
import weka.core.converters.{CSVLoader, ArffLoader}
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.linkspec.condition.{FlatIndexClassifierAggregator, MultiIndexAggregator, FeatureInstance, MultiIndexClassifierAggregator}

@StrategyAnnotation(id = "weka", label = "WekaClassifier", description = "Classifier implementation that uses a serialized weka classifier")
class WekaClassifierAggregator(classifierFileName: String, arffDatasetFileName: String = "__no_file__", csvDatasetFileName: String = "__no_file__", classField: String) extends FlatIndexClassifierAggregator
{
  private val logger = Logger.getLogger(classOf[WekaClassifierAggregator].getName)
  val serializedClassifierFile = new File(classifierFileName)
  val arffDatasetFile = new File(arffDatasetFileName)
  val csvDatasetFile = new File(csvDatasetFileName)
  var wekaDatasetStructure: weka.core.Instances = null
  var wekaClassifier: Classifier = null
  var initialized = false
  var classAttributeTrueIndex = 0;
  var classAttributeFalseIndex = 0;

  private def initialize() =
  {
    logger.info("loading classifier")
    val ois = new ObjectInputStream(new FileInputStream(this.serializedClassifierFile))
    this.wekaClassifier = ois.readObject().asInstanceOf[Classifier]
    ois.close()

    logger.info("loading data structure")
    logger.info("perferred: arff file " + arffDatasetFile)
    logger.info("fallback:   csv file " + csvDatasetFile)
    if (arffDatasetFile.exists) {
      logger.info("loading data structure from existing arff file")
      val arffLoader = new ArffLoader()
      arffLoader.setSource(arffDatasetFile)
      this.wekaDatasetStructure = arffLoader.getStructure()
    } else if (csvDatasetFile.exists) {
      logger.info("loading data structure from existing csv file (may take a while)")
      // load CSV
      val loader = new CSVLoader();
      loader.setSource(csvDatasetFile);
      this.wekaDatasetStructure = loader.getStructure();
      //      logger.info("saving data structure to arff file for future use")
      //      // save ARFF
      //      ArffSaver saver = new ArffSaver();
      //      saver.setInstances(data);
      //      saver.setFile(new File(args[1]));
      //      saver.setDestination(new File(args[1]));
      //      saver.writeBatch();

    } else {
      throw new IllegalArgumentException("did not find a structure file (csv nor arff)")
    }
    this.wekaDatasetStructure.setClass(this.wekaDatasetStructure.attribute(this.classField))
    logger.info("finished setting up the classifier")
    logger.info("dataset has " + this.wekaDatasetStructure.numAttributes + " attributes")
    for (i <- List.range(0, this.wekaDatasetStructure.numAttributes)) {
      logger.info("attribute " + i + ": " + this.wekaDatasetStructure.attribute(i))
    }
    this.classAttributeTrueIndex = this.wekaDatasetStructure.classAttribute.indexOfValue("true")
    this.classAttributeFalseIndex = this.wekaDatasetStructure.classAttribute.indexOfValue("false")

    this.initialized = true
  }

  override def classify(featureVector: Traversable[Option[FeatureInstance]]) =
  {
    try {
      if (!this.initialized) {
        initialize()
      }
      if ((featureVector.isEmpty || featureVector.exists(_.isEmpty))) {
        None
      }
      else {
        //logger.info("weka classifier: creating instance")
        val instanceOption = createInstanceFromFeatureVector(featureVector)
        if (instanceOption.isEmpty) {
          None
        }
        val instance = instanceOption.get
        //logger.info("instance: " + instance)
        //logger.info("weka classifier: classifying instance")
        val classifierResult = this.wekaClassifier.distributionForInstance(instance)
        if (classifierResult.length > 2) {
          logger.severe("cannot handle classification result for more than 2 classes, omitting")
          None
        } else {
          if (classifierResult.length == 0) {
            logger.severe("cannot handle zero-sized classification result omitting")
            None
          } else {
            if (classifierResult.length == 1) {
              //logger.info("weka classifier: classification result = 0 (" + classifierResult(0)+")")
              Some(math.min(1.0,math.max(0.0,classifierResult(0))))
            } else {

              //length is 2
              //logger.info("weka classifier: true attribute index=" + this.classAttributeTrueIndex + ", confidence=" + classifierResult(this.classAttributeTrueIndex))
              if (classifierResult(this.classAttributeTrueIndex) > classifierResult(this.classAttributeFalseIndex)) {
                //logger.info("weka classifier: returning true class with confidence " + classifierResult(this.classAttributeTrueIndex))
                //logger.info("weka classifier: complete result: size=" + classifierResult.size + "; data=(" + classifierResult(0) + ", " + classifierResult(1)+")")
                Some(math.min(1.0,math.max(0.0,classifierResult(this.classAttributeTrueIndex))))
              } else {
                //logger.info("weka classifier: returning false")
                Some(0.0)
              }
            }
          }
        }
      }
    } catch {
      case e: Exception => {
        logger.severe("cannot classify instance " + featureVector + ", " + e.getMessage)
        Some(0.0)
      }
    }
  }

  /**
   * Creates an Option of weka instance from the specified feature vector or None if the instance cannot be created
   */
  private def createInstanceFromFeatureVector(featureVector: Traversable[Option[FeatureInstance]]): Option[weka.core.Instance] =
  {
    //logger.info("creating weka instance for feature vector" + featureVector)
    if (featureVector.size != this.wekaDatasetStructure.numAttributes) {
      throw new IllegalArgumentException("cannot create weka Instance from feature vector: different number of attributes, expected " + this.wekaDatasetStructure.numAttributes + ", actual: " + featureVector.size)
    }
    val wekaInstance: weka.core.Instance = new weka.core.DenseInstance(featureVector.size)
    wekaInstance.setDataset(this.wekaDatasetStructure)


    var index = 0
    //there cannot be a None value in the feature vector, so we can safely use Some(featureVector) to
    //work with the featureInstance directly
    try {
      for (Some(feature) <- featureVector) {
        //logger.info("handling feature" + feature)
        //missing feature is handled in the classify method.
        // Missing feature value is handled here
        val wekaAttribute = this.wekaDatasetStructure.attribute(index);
        if (feature.value.isEmpty) {
          //logger.info("setting missing value on index " + index)
          //atts(index) = wekaAttribute.
          wekaInstance.setMissing(wekaAttribute);
        } else {
          try {
            //logger.info("setting value '" + feature.value.get + "' of attribute " + wekaAttribute + ", index " + index)
            feature.dataType match {
              case "numeric" => wekaInstance.setValue(wekaAttribute, feature.value.get.toDouble)
              case "nominal" => wekaInstance.setValue(wekaAttribute, feature.value.get)
              case "boolean" => wekaInstance.setValue(wekaAttribute, feature.value.get.toBoolean.toString)
              case "ordinal" => wekaInstance.setValue(wekaAttribute, feature.value.get.toInt.toString)
              case _ => {
                logger.severe("can't handle feature dataType '" + feature.dataType + "' in feature vector " + featureVector + ", treating as missing")
                wekaInstance.setMissing(wekaAttribute);
              }
            }
          } catch {
            case e: Exception => {
              //logger.severe("cannot create weka attribute, treating as missing. Feature=" + feature + ", cause: " + e.getMessage)
              wekaInstance.setMissing(wekaAttribute);
            }
          }
        }
        index = index + 1
      }
    } catch {
      case e: Exception => {
        logger.severe("cannot create weka instance for feature vector " + featureVector + ", cause: " + e.getMessage)
        None
      }
    }
    //logger.info("created instance" + wekaInstance)
    Some(wekaInstance)
  }


}