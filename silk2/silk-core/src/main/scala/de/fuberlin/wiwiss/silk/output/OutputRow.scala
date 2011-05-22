package de.fuberlin.wiwiss.silk.output

import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import de.fuberlin.wiwiss.silk.linkspec.input.Input
import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.linkspec.condition.{Feature, FeatureInstance}


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

trait Fields
{
  def getHeader(): Seq[String]

  def apply(instances: SourceTargetPair[Instance],
            featureVector: Traversable[Option[FeatureInstance]],
            classificationResult: Option[Double]): Seq[String]
}

class InputFields(name: String, val inputs: Seq[Input]) extends Fields
{
  override def getHeader(): Seq[String] = Seq(name)

  override def apply(instances: SourceTargetPair[Instance],
                     featureVector: Traversable[Option[FeatureInstance]],
                     classificationResult: Option[Double]): Seq[String] =
  {
    Seq(inputs.flatMap(_.apply(instances)).mkString(","))
  }
}

class ClassificationResultFields(name: String) extends Fields
{
  override def getHeader(): Seq[String] = Seq(name)

  override def apply(instances: SourceTargetPair[Instance],
                     featureVector: Traversable[Option[FeatureInstance]],
                     classificationResult: Option[Double]): Seq[String] =
  {
    if (classificationResult.isDefined) {
      Seq(classificationResult.get.toString)
    } else {
      Seq("")
    }
  }
}

class FeatureVectorFields(features: Traversable[Feature], missingValue: String = "?", omitFeatures: Seq[String] = Seq.empty, selectFeatures: Seq[String]= Seq.empty) extends Fields
{
  override def getHeader(): Seq[String] =
  {
    val filteredFeatures=
      if (!selectFeatures.isEmpty){
        //output only selected features
        features.filter((x:Feature) => this.selectFeatures.contains(x.featureName))
      } else if (!omitFeatures.isEmpty) {
        //output all except omitFeatures
        features.filter((x:Feature) => !this.omitFeatures.contains(x.featureName))
      } else {
        //output all features
        features
      }
    filteredFeatures.map(_.featureName).toSeq
  }

  override def apply(instances: SourceTargetPair[Instance],
                     featureVector: Traversable[Option[FeatureInstance]],
                     classificationResult: Option[Double]): Seq[String] =
  {

    val filteredFeatures =
      if (!selectFeatures.isEmpty){
        //output only selected features
        featureVector.filter
            {
              case Some(feature) => this.selectFeatures.contains(feature.featureName)
              case _ => false
            }
      } else if (!omitFeatures.isEmpty) {
        //output all except omitFeatures
        featureVector.filter
            {
              case Some(feature) => !this.omitFeatures.contains(feature.featureName)
              case _ => false
            }
      } else {
        //output all features
        featureVector
      }

    filteredFeatures.map
          {
            case Some(feature) => makeFeatureInstanceString(feature)
            case _ => missingValue
          }.toSeq
  }

  private def makeFeatureInstanceString(feature: FeatureInstance): String =
  {
    if (feature.value.isEmpty) {
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
}

class OutputRow(val fields: Seq[Fields])
{
  def getHeader(): Seq[String] =
  {
    this.fields.map(_.getHeader).flatten
  }

  def apply(instances: SourceTargetPair[Instance],
            featureVector: Traversable[Option[FeatureInstance]],
            classificationResult: Option[Double]): Seq[String] =
  {
    //walk over fields and create a seq
    fields.map(_.apply(instances, featureVector, classificationResult)).flatten
  }
}