package de.fuberlin.wiwiss.silk.linkspec.input

import de.fuberlin.wiwiss.silk.instance.{Instance, Path}
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import de.fuberlin.wiwiss.silk.config.Prefixes

/**
 * A PathInput retrieves values from a data item by a given RDF path and optionally applies a transformation to them.
 */
case class PathInput(path : Path) extends Input
{
  /** The cached index of this path in the instance specification */
  private var pathIndex = -1

  private var sourcePathCount = -1

  /**
   * Retrieves the values of this input for a given instance.
   *
   * @param instances The pair of instances.
   * @return The values.
   */
  override def apply(instances : SourceTargetPair[Instance]) =
  {
    if (path.operators.isEmpty){
      //the path only consists of ?[variable]
      if(instances.source.spec.variable == path.variable)
      {
        Set(instances.source.uri)
      }
      else if(instances.target.spec.variable == path.variable)
      {
        Set(instances.target.uri)
      }
      else
      {
        Traversable.empty
      }
    } else {
      if(pathIndex == -1 || sourcePathCount != instances.source.spec.paths.size)
      {
        if(instances.source.spec.variable == path.variable)
        {
          pathIndex = instances.source.spec.pathIndex(path)
        }
        else if(instances.target.spec.variable == path.variable)
        {
          pathIndex = instances.target.spec.pathIndex(path)
        }
  
        sourcePathCount = instances.source.spec.paths.size
      }
      if(instances.source.spec.variable == path.variable)
      {
        instances.source.evaluate(pathIndex)
      }
      else if(instances.target.spec.variable == path.variable)
      {
        instances.target.evaluate(pathIndex)
      }
      else
      {
        Traversable.empty
      }
    }
  }

  override def toXML(implicit prefixes : Prefixes) = <Input path={path.serialize} />
}
