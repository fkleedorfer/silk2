package de.fuberlin.wiwiss.silk.instance

import de.fuberlin.wiwiss.silk.linkspec._
import condition._
import input.{TransformInput, PathInput, Input}
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import xml.Node
import de.fuberlin.wiwiss.silk.config.Prefixes
import de.fuberlin.wiwiss.silk.output.InputFields

class InstanceSpecification(val variable : String, val restrictions : Restrictions, val paths : Seq[Path])
{
  def pathIndex(path : Path) =
  {
    paths.indexWhere(_ == path) match
    {
      case -1 => throw new NoSuchElementException("Path " + path + " not found on instance.")
      case index => index
    }
  }

  override def toString = "InstanceSpecification(variable='" + variable + "' restrictions='" + restrictions + "' paths=" + paths + ")"

  def toXML =
  {
    <InstanceSpecification>
      <Variable>{variable}</Variable>
      { restrictions.toXML }
      <Paths>{
        for(path <- paths) yield
        {
          <Path>{path.serialize(Prefixes.empty)}</Path>
        }
      }</Paths>
    </InstanceSpecification>
  }
}

object InstanceSpecification
{
  def fromXML(node : Node) =
  {
    new InstanceSpecification(
      variable = node \ "Variable" text,
      restrictions = Restrictions.fromXML(node \ "Restrictions" head)(Prefixes.empty),
      paths = for(pathNode <- node \ "Paths" \ "Path") yield Path.parse(pathNode text, Prefixes.empty)
    )
  }

  def retrieve(linkSpec : LinkSpecification) : SourceTargetPair[InstanceSpecification] =
  {
    val sourceVar = linkSpec.datasets.source.variable
    val targetVar = linkSpec.datasets.target.variable

    val sourceRestriction = linkSpec.datasets.source.restriction
    val targetRestriction = linkSpec.datasets.target.restriction

    val sourcePaths = linkSpec.condition.rootOperator match
    {
      case Some(operator) => collectPaths(sourceVar)(operator)
      case None => Set[Path]()
    }

    val targetPaths = linkSpec.condition.rootOperator match
    {
      case Some(operator:Operator) => collectPaths(targetVar)(operator)
      case None => Set[Path]()
    }

    val sourceInstanceSpec = new InstanceSpecification(sourceVar, sourceRestriction, sourcePaths.toSeq)
    val targetInstanceSpec = new InstanceSpecification(targetVar, targetRestriction, targetPaths.toSeq)

    SourceTargetPair(sourceInstanceSpec, targetInstanceSpec)
  }


  private def collectPaths(variable : String)(operator : Operator) : Set[Path] = operator match
  {
    case classifierAggregation : ClassifierAggregation => {
      (classifierAggregation.features.flatMap(collectPathsFromFeature(variable))
      ++
      classifierAggregation.featureVectorHandlers.flatMap(collectPathsFromFeatureVectorHandler(variable))).toSet
    }

    case aggregation : Aggregation => aggregation.operators.flatMap(collectPaths(variable)).toSet
    case comparison : Comparison =>
    {
      val sourcePaths = collectPathsFromInput(variable)(comparison.inputs.source)
      val targetPaths = collectPathsFromInput(variable)(comparison.inputs.target)
      (sourcePaths ++ targetPaths).toSet
    }
    case randomOperator: RandomOperator => Set()
  }

  private def collectPathsFromInput(variable : String)(param : Input) : Set[Path] = param match
  {
    case p : PathInput if p.path.variable == variable => Set(p.path)
    case p : TransformInput => p.inputs.flatMap(collectPathsFromInput(variable)).toSet
    case _ => Set()
  }

  /** added for classification **/
  private def collectPathsFromFeature(variable : String)(feature : Feature) : Set[Path] = feature match
  {
    case opfeature : OperatorFeature => (collectPaths(variable)(opfeature.operator)).toSet
    case exfeature : ExtractorFeature => {
      val sourcePaths = collectPathsFromInput(variable)(exfeature.inputs.source)
      val targetPaths = collectPathsFromInput(variable)(exfeature.inputs.target)
      (sourcePaths ++ targetPaths).toSet
    }
  }

  /** added for classification **/
  private def collectPathsFromFeatureVectorHandler(variable : String)(handler : FeatureVectorHandler) : Set[Path] =
  {
    handler.outputRows.flatMap{
      _.fields.collect{
        case inputFields:InputFields =>
            inputFields.inputs.flatMap((input:Input) => ( collectPathsFromInput(variable)(input):Set[Path]))
      }.flatten
    }.toSet
  }

}
