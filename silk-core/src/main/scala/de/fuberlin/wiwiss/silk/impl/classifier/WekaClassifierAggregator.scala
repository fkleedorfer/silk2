package de.fuberlin.wiwiss.silk.impl.classifier

import de.fuberlin.wiwiss.silk.linkspec.MultiIndexAggregator
import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import de.fuberlin.wiwiss.silk.linkspec.condition.{FeatureInstance, MultiIndexClassifierAggregator}
import weka.classifiers.Classifier
import java.io.{File, ObjectInputStream, FileInputStream}
import weka.core.converters.{CSVLoader, ArffLoader}
import java.util.logging.Logger

@StrategyAnnotation(id = "weka", label = "WekaClassifier", description = "Classifier implementation that uses a serialized weka classifier")
class WekaClassifierAggregator(classifierFileName: String, arffDatasetFileName: String, csvDatasetFileName: String, classField:String) extends MultiIndexClassifierAggregator
{
  private val logger = Logger.getLogger(classOf[WekaClassifierAggregator].getName)
  val serializedClassifierFile = new File(classifierFileName)
  val arffDatasetFile = new File(arffDatasetFileName)
  val csvDatasetFile = new File(csvDatasetFileName)
  var wekaDatasetStructure: weka.core.Instances = null
  var wekaClassifier: Classifier = null
  var initialized= false

  private def initialize() =
  {
    logger.info("loading classifier")
    val ois = new ObjectInputStream(new FileInputStream(this.serializedClassifierFile))
    this.wekaClassifier = ois.readObject().asInstanceOf[Classifier]
    ois.close()

    logger.info("loading data structure")
    logger.info("perferred: arff file " + arffDatasetFile)
    logger.info("fallback:   csv file " + csvDatasetFile)
    if (arffDatasetFile.exists){
      logger.info("loading data structure from existing arff file")
      val arffLoader = new ArffLoader()
      arffLoader.setSource(arffDatasetFile)
      this.wekaDatasetStructure = arffLoader.getStructure()
    } else if (csvDatasetFile.exists){
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
    for (i <- List.range(0,this.wekaDatasetStructure.numAttributes)){
      logger.info("attribute " + i + ": " + this.wekaDatasetStructure.attribute(i))
    }


    this.initialized = true
  }

  override def classify(featureVector : Traversable[Option[FeatureInstance]]) =
  {
    if (!this.initialized) {
      initialize()
    }
    if((featureVector.isEmpty || featureVector.exists(_.isEmpty)))
    {
      None
    }
    else
    {
      //logger.info("weka classifier: creating instance")
      val instance = createInstanceFromFeatureVector(featureVector)
      //logger.info("weka classifier: classifying instance")
      val classifierResult = this.wekaClassifier.classifyInstance(instance)
      logger.info("weka classifier: classification result = " + classifierResult)
      Some(classifierResult)
    }
  }

  private def createInstanceFromFeatureVector(featureVector: Traversable[Option[FeatureInstance]]) : weka.core.Instance =
  {
    //logger.info("creating weka instance for feature vector" + featureVector)
    if (featureVector.size != this.wekaDatasetStructure.numAttributes){
      throw new IllegalArgumentException("cannot create weka Instance from feature vector: different number of attributes, expected " + this.wekaDatasetStructure.numAttributes + ", actual: " + featureVector.size)
    }
    val wekaInstance:weka.core.Instance = new weka.core.DenseInstance(featureVector.size)
    wekaInstance.setDataset(this.wekaDatasetStructure)


    var index = 0
    //there cannot be a None value in the feature vector, so we can safely use Some(featureVector) to
    //work with the featureInstance directly
    for(Some(feature) <- featureVector)
      {
        //logger.info("handling feature" + feature)
        //missing feature is handled in the classify method.
        // Missing feature value is handled here
        val wekaAttribute = this.wekaDatasetStructure.attribute(index);
        if (feature.value.isEmpty){
          //logger.info("setting missing value on index " + index)
          //atts(index) = wekaAttribute.
          wekaInstance.setMissing(wekaAttribute);
        } else {
          //logger.info("setting value '" + feature.value.get + "' of attribute " + wekaAttribute + ", index " + index)
          feature.dataType match {
            case "numeric" => wekaInstance.setValue(wekaAttribute, feature.value.get.toDouble)
            case "nominal" => wekaInstance.setValue(wekaAttribute, feature.value.get)
            case "boolean" => wekaInstance.setValue(wekaAttribute, feature.value.get.toBoolean.toString)
            case "ordinal" => wekaInstance.setValue(wekaAttribute, feature.value.get.toInt.toString)
            case _ => throw new IllegalArgumentException("can't handle feature dataType '" + feature.dataType + "' in feature vector " + featureVector )
           }
        }
        index = index + 1
      }
    //logger.info("created instance" + wekaInstance)
    wekaInstance
  }


}