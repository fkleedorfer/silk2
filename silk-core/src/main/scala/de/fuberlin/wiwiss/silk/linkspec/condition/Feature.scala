package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import de.fuberlin.wiwiss.silk.impl.metric.EqualityMetric
import de.fuberlin.wiwiss.silk.linkspec.input.Input
import java.util.logging.Logger

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

  def apply(instances : SourceTargetPair[Instance], threshold: Double) : Option[FeatureInstance]

  def index(instance : Instance, threshold : Double) : Set[Seq[Int]]

  val blockCounts : Seq[Int]
}

/**
 * Extracts a feature iff both inputs contain at least one identity. The value of the feature is the
 * value contained in both sources. If there are more such values, one is chosen at random.
 */
case class ExtractorFeature(featureName: String, dataType: String, required: Boolean, params: Map[String, String], inputs : SourceTargetPair[Input]) extends Feature
{
  private val logger = Logger.getLogger(classOf[ExtractorFeature].getName)
  private val metric = new EqualityMetric()

  override def apply(instances : SourceTargetPair[Instance], threshold:Double) : Option[FeatureInstance] =
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
        if (required){
          None
        } else {
          Some(FeatureInstance(featureName, dataType, None))
        }
      } else {
        Some(FeatureInstance(featureName, dataType, Some(equalValues.head.get.toString)))
      }
    } else {
      None
    }
  }

  override val blockCounts = metric.blockCounts

  override def index(instance : Instance, threshold : Double) : Set[Seq[Int]] =
  {

   val values = inputs.source(SourceTargetPair(instance, instance)) ++ inputs.target(SourceTargetPair(instance, instance))
    values.flatMap(value => metric.index(value, threshold)).toSet
  }

  override def toString =
  {
    "ExtractorFeature(type=extractor, required=" + required + ", datatype=" + dataType + ", inputs=" + inputs + ")"
  }
}

case class OperatorFeature(featureName: String, dataType: String, required: Boolean, params: Map[String, String], operator: Operator ) extends Feature
{
  private val logger = Logger.getLogger(classOf[OperatorFeature].getName)

  override def apply(instances : SourceTargetPair[Instance], threshold : Double) : Option[FeatureInstance] =
  {
    val operatorResult = operator.apply(instances,threshold);
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
    operator.index(instance, threshold)
  }

  override val blockCounts = operator.blockCounts

  override def toString =
  {
    "OperatorFeature(type=operator, required=" + required + ", operator=" + operator + ")"
  }
}

case class FeatureInstance (featureName: String, dataType: String, value: Option[String])
