package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import de.fuberlin.wiwiss.silk.impl.metric.EqualityMetric
import de.fuberlin.wiwiss.silk.linkspec.input.Input
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.util.strategy.{Strategy, Factory, StrategyAnnotation}

/**
 * <DESCRIPTION>
 * 
 * <p>
 * <b>Company:&nbsp;</b> SAT, Research Studios Austria
 * </p>
 * 
 * <p>
 * <b>Copyright:&nbsp;</b> (c) 2011
 * </p>
 * 
 * <p>
 * <b>last modified:</b><br/>
 * $Author: $<br/>
 * $Date: $<br/>
 * $Revision: $
 * </p>
 * 
 * @author Florian Kleedorfer
 */
trait Feature
{
  val featureName: String

  val dataType: String

  val required: Boolean

  val disableBlocking: Boolean

  def apply(instances : SourceTargetPair[Instance], threshold: Double) : Option[FeatureInstance]

  def index(instance : Instance, threshold : Double) : Set[Seq[Int]] = Set(Seq(0))

  val blockCounts : Seq[Int] = Seq(1)
}

trait Extractor extends Strategy
{
  def apply(instances : SourceTargetPair[Instance], inputs: SourceTargetPair[Input], threshold: Double) : Option[String]

  def index(value : String, threshold : Double = 0.0) : Set[Seq[Int]] = Set(Seq(0))

  val blockCounts : Seq[Int] = Seq(1)
}



object Extractor extends Factory[Extractor]



/**
 * Extracts the first value from either the first or second input (as configured with useFirstInput=[true|false]
 */
@StrategyAnnotation(id = "single", label = "Single Value Extractor", description = "Extracts the first value of one of the inputs")
class SingleValueExtractor(useFirstInput: Boolean) extends Extractor
{
  private val logger = Logger.getLogger(classOf[ExtractorFeature].getName)

  def apply(instances : SourceTargetPair[Instance], inputs: SourceTargetPair[Input], threshold: Double) : Option[String] =
  {
    val set1 = inputs.source(instances)
    val set2 = inputs.target(instances)

    if (useFirstInput){
      if (set1.isEmpty) {
          None
      } else {
        Some(set1.head)
      }
    } else {
       if (set2.isEmpty) {
          None
      } else {
        Some(set2.head)
      }
    }
  }
}

/**
 * Extracts the first value from either the first or second input (as configured with useFirstInput=[true|false]
 */
@StrategyAnnotation(id = "identity", label = "Identical Value Extractor", description = "Searches both inputs for identical values and outputs the first one found")
class IdenticalValuesExtractor() extends Extractor
{
  private val logger = Logger.getLogger(classOf[ExtractorFeature].getName)
  private val metric = new EqualityMetric()
  def apply(instances : SourceTargetPair[Instance], inputs: SourceTargetPair[Input], threshold: Double) : Option[String] =
  {
    val set1 = inputs.source(instances)
    val set2 = inputs.target(instances)

    //logger.info("extractionFeature.apply: threshold=" + threshold + ", set1=" + set1 + ", set2=" + set2)
    if(!set1.isEmpty && !set2.isEmpty)
    {
      val identicalValues= for (str1 <- set1; str2 <- set2) yield
        (
          if (metric.evaluate(str1, str2, threshold) == 1) {
           Some(str1)
          } else {
            None
          })
      val equalValues = identicalValues.filter{
        case Some(x) => true
        case None => false
      }
      if (equalValues.size == 0) {
        None
      } else {
        Some(equalValues.head.get.toString)
      }
    } else {
      None
    }
  }

  override def index(value : String, threshold : Double = 0.0) : Set[Seq[Int]] = metric.index(value, threshold).toSet

  override val blockCounts = metric.blockCounts
}

/**
 * Extracts the smallest value from the inputs
 */
@StrategyAnnotation(id = "min", label = "Minimum Value Extractor", description = "Extracts the smallest value from the inputs")
class MinimumValuesExtractor() extends Extractor
{
  private val logger = Logger.getLogger(classOf[ExtractorFeature].getName)
  def apply(instances : SourceTargetPair[Instance], inputs: SourceTargetPair[Input], threshold: Double) : Option[String] =
  {
    //logger.info("comparing geocode quality")
    val set1 = inputs.source(instances)
    val set2 = inputs.target(instances)

    //logger.info("extractionFeature.apply: threshold=" + threshold + ", set1=" + set1 + ", set2=" + set2)
    if(!set1.isEmpty && !set2.isEmpty)
    {
      val allValues = set1 ++ set2
      //logger.info("computing minimum of geocodes:" + allValues + ": " + allValues.min)
      Some(allValues.min.toString)
    } else {
      None
    }
  }
}

/**
 * Extracts a feature iff both inputs contain at least one identity. The value of the feature is the
 * value contained in both sources. If there are more such values, one is chosen at random.
 */
case class ExtractorFeature(featureName: String, dataType: String, required: Boolean, disableBlocking: Boolean = false, params: Map[String, String], inputs : SourceTargetPair[Input], extractor: Extractor) extends Feature
{
  private val logger = Logger.getLogger(classOf[ExtractorFeature].getName)

  override def apply(instances : SourceTargetPair[Instance], threshold:Double) : Option[FeatureInstance] =
  {
    val result = extractor.apply(instances, inputs, threshold)
    if (result.isDefined){
      Some(FeatureInstance(featureName, dataType, result))
    } else {
      if (required){
        None
      } else {
        Some(FeatureInstance(featureName, dataType, None))
      }
    }
  }

  override val blockCounts = {
    if (disableBlocking){
      Seq(1)
    } else {
      extractor.blockCounts
    }
  }

  override def index(instance : Instance, threshold : Double) : Set[Seq[Int]] =
  {
    if (disableBlocking){
      Set(Seq(0))
    } else {
      val values = inputs.source(SourceTargetPair(instance, instance)) ++ inputs.target(SourceTargetPair(instance, instance))
      values.flatMap(value => extractor.index(value, threshold)).toSet
    }
  }

  override def toString =
  {
    "ExtractorFeature(type=extractor, required=" + required + ", datatype=" + dataType + ", inputs=" + inputs + ", disableBlocking=" + disableBlocking +")"
  }
}

case class OperatorFeature(featureName: String, dataType: String, required: Boolean, disableBlocking: Boolean = false, params: Map[String, String], operator: Operator ) extends Feature
{
  private val logger = Logger.getLogger(classOf[OperatorFeature].getName)

  override def apply(instances : SourceTargetPair[Instance], threshold : Double) : Option[FeatureInstance] =
  {
    val operatorResult = operator.apply(instances,threshold);
    //System.out.println("featureName:" + featureName + ", result:" + operatorResult)
    if (operatorResult.isEmpty)
      if (required){
        None
      } else {
      Some(new FeatureInstance(featureName, dataType, None))
    } else {
      dataType match {
        case "boolean" => Some(new FeatureInstance(featureName, dataType, Some(makeBooleanString(operatorResult.get))))
        case _ => Some(new FeatureInstance(featureName, dataType, Some(operatorResult.get.toString)))
      }
    }
  }

  private def makeBooleanString(value: Double):String =
  {
    (value == 1.0).toString
  }

  override def index(instance : Instance, threshold : Double) : Set[Seq[Int]] =
  {
    if (disableBlocking) {
      Set(Seq(0))
    } else {
      operator.index(instance, threshold)
    }
  }

  override val blockCounts = {
    if (disableBlocking){
      Seq(1)
    } else {
      operator.blockCounts
    }
  }

  override def toString =
  {
    "OperatorFeature(type=operator, required=" + required + ", disableBlocking=" + disableBlocking + ", operator=" + operator + ")"
  }
}

case class FeatureInstance (featureName: String, dataType: String, value: Option[String])
