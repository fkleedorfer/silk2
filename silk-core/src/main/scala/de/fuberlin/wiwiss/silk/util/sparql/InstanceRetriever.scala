package de.fuberlin.wiwiss.silk.util.sparql

import de.fuberlin.wiwiss.silk.instance.{InstanceSpecification, Instance}

/**
 * Retrieves instances from a SPARQL endpoint.
 */
trait InstanceRetriever
{
   /**
   * Retrieves instances with a given instance specification.
   *
   * @param instanceSpec The instance specification
   * @param instances The URIs of the instances to be retrieved. If empty, all instances will be retrieved.
   * @return The retrieved instances
   */
  def retrieve(instanceSpec : InstanceSpecification, instances : Seq[String]) : Traversable[Instance]
}

/**
 * Factory for creating InstanceRetriever instances.
 */
object InstanceRetriever
{
  /**
   * Creates a new InstanceRetriever instance.
   */
  def apply(endpoint : SparqlEndpoint, pageSize : Int = 1000, graphUri : Option[String] = None) : InstanceRetriever =
  {
    //Always creates a parallel instance retriever as it is generally significantly faster.
    //new ParallelInstanceRetriever(endpoint, pageSize, graphUri)
    new SimpleInstanceRetriever(endpoint,pageSize, graphUri)
  }
}
