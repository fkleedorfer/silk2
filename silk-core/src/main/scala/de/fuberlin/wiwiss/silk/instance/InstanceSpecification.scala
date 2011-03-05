package de.fuberlin.wiwiss.silk.instance

import de.fuberlin.wiwiss.silk.linkspec._
import condition.{Operator, Comparison, Aggregation}
import input.{TransformInput, PathInput, Input}
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import xml.Node
import de.fuberlin.wiwiss.silk.config.Prefixes

class InstanceSpecification(val variable : String, val restrictions : String, val paths : Seq[Path], val prefixes : Prefixes)
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
      <Restrictions>{restrictions}</Restrictions>
      <Paths>{
        for(path <- paths) yield
        {
          <Path>{path.toString}</Path>
        }
      }</Paths>
      <Prefixes>{
        for((key, value) <- prefixes) yield
        {
          <Prefix id={key} namespace={value} />
        }
      }</Prefixes>
    </InstanceSpecification>
  }
}

object InstanceSpecification
{
  def fromXML(node : Node) =
  {
    val prefixes = Prefixes((node \ "Prefixes" \ "Prefix").map(n => (n \ "@id" text, n \ "@namespace" text)).toMap)

    new InstanceSpecification(
      variable = node \ "Variable" text,
      restrictions = node \ "Restrictions" text,
      paths = for(pathNode <- node \ "Paths" \ "Path") yield Path.parse(pathNode text, prefixes),
      prefixes = prefixes
    )
  }

  def retrieve(linkSpec : LinkSpecification, prefixes : Prefixes) : SourceTargetPair[InstanceSpecification] =
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
      case Some(operator) => collectPaths(targetVar)(operator)
      case None => Set[Path]()
    }

    val sourceInstanceSpec = new InstanceSpecification(sourceVar, sourceRestriction, sourcePaths.toSeq, prefixes)
    val targetInstanceSpec = new InstanceSpecification(targetVar, targetRestriction, targetPaths.toSeq, prefixes)

    SourceTargetPair(sourceInstanceSpec, targetInstanceSpec)
  }

  private def collectPaths(variable : String)(operator : Operator) : Set[Path] = operator match
  {
    case aggregation : Aggregation => aggregation.operators.flatMap(collectPaths(variable)).toSet
    case comparison : Comparison =>
    {
      val sourcePaths = collectPathsFromInput(variable)(comparison.inputs.source)
      val targetPaths = collectPathsFromInput(variable)(comparison.inputs.target)
      (sourcePaths ++ targetPaths).toSet
    }
  }

  private def collectPathsFromInput(variable : String)(param : Input) : Set[Path] = param match
  {
    case p : PathInput if p.path.variable == variable => Set(p.path)
    case p : TransformInput => p.inputs.flatMap(collectPathsFromInput(variable)).toSet
    case _ => Set()
  }
}
