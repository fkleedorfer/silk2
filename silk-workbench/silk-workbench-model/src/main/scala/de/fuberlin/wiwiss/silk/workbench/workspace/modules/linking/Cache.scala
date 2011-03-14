package de.fuberlin.wiwiss.silk.workbench.workspace.modules.linking

import de.fuberlin.wiwiss.silk.instance.{InstanceSpecification, Instance}
import xml.{NodeBuffer, Node}
import de.fuberlin.wiwiss.silk.util.{Task, SourceTargetPair}
import de.fuberlin.wiwiss.silk.util.sparql.{InstanceRetriever, SparqlEndpoint}
import de.fuberlin.wiwiss.silk.config.Prefixes

//TODO use options?
//TODO store path frequencies
class Cache(var instanceSpecs : SourceTargetPair[InstanceSpecification] = null,
            var instances : ReferenceInstances = null)
{
  def toXML(implicit prefixes : Prefixes) : Node =
  {
    val nodes = new NodeBuffer()

    if(instanceSpecs != null)
    {
      nodes.append(
        <InstanceSpecifications>
          <Source>
            { instanceSpecs.source.toXML }
          </Source>
          <Target>
            { instanceSpecs.target.toXML }
          </Target>
        </InstanceSpecifications>)
    }

    if(instances != null)
    {
      nodes.append(
          <PositiveInstances>{
          for(SourceTargetPair(sourceInstance, targetInstance) <- instances.positiveInstances) yield
          {
            <Pair>
              <Source>{sourceInstance.toXML}</Source>
              <Target>{targetInstance.toXML}</Target>
            </Pair>
          }
          }</PositiveInstances>)

      nodes.append(
        <NegativeInstances>{
          for(SourceTargetPair(sourceInstance, targetInstance) <- instances.negativeInstances) yield
          {
            <Pair>
              <Source>{sourceInstance.toXML}</Source>
              <Target>{targetInstance.toXML}</Target>
            </Pair>
          }
        }</NegativeInstances>)
    }

    <Cache>{nodes}</Cache>
  }
}

object Cache
{
  def fromXML(node : Node) : Cache =
  {
    val instanceSpecs =
    {
      if(node \ "InstanceSpecifications" isEmpty)
      {
        null
      }
      else
      {
        val sourceSpec = InstanceSpecification.fromXML(node \ "InstanceSpecifications" \ "Source" \ "_" head)
        val targetSpec = InstanceSpecification.fromXML(node \ "InstanceSpecifications" \ "Target" \ "_" head)
        new SourceTargetPair(sourceSpec, targetSpec)
      }
    }

    val positiveInstances =
    {
      if(node \ "PositiveInstances" isEmpty)
      {
        null
      }
      else
      {
        for(pairNode <- node \ "PositiveInstances" \ "Pair" toList) yield
        {
           SourceTargetPair(
             Instance.fromXML(pairNode \ "Source" \ "Instance" head, instanceSpecs.source),
             Instance.fromXML(pairNode \ "Target" \ "Instance" head, instanceSpecs.target))
        }
      }
    }

    val negativeInstances =
    {
      if(node \ "NegativeInstances" isEmpty)
      {
        null
      }
      else
      {
        for(pairNode <- node \ "NegativeInstances" \ "Pair" toList) yield
        {
           SourceTargetPair(
             Instance.fromXML(pairNode \ "Source" \ "Instance" head, instanceSpecs.source),
             Instance.fromXML(pairNode \ "Target" \ "Instance" head, instanceSpecs.target))
        }
      }
    }

    val instances = if(positiveInstances != null && negativeInstances != null) ReferenceInstances(positiveInstances, negativeInstances) else null

    new Cache(instanceSpecs, instances)
  }
}
