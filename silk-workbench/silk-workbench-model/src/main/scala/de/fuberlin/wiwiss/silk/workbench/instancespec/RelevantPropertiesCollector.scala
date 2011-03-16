package de.fuberlin.wiwiss.silk.workbench.instancespec

import de.fuberlin.wiwiss.silk.util.sparql.SparqlEndpoint
import de.fuberlin.wiwiss.silk.util.Uri
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.instance.{Path, ForwardOperator}
import de.fuberlin.wiwiss.silk.linkspec.Restrictions

/**
 * Retrieves the most frequent property paths.
 */
object RelevantPropertiesCollector
{
  /** The minimum frequency of a property to be considered relevant */
  private val MinFrequency = 0.7

  /** The maximum number of relevant properties */
  private val MaxPropertyCount = 50

  private val logger = Logger.getLogger(RelevantPropertiesCollector.getClass.getName)

  /**
   * Retrieves a list of properties which are defined on most instances.
   */
  def apply(endpoint : SparqlEndpoint, restrictions : Restrictions) : Traversable[(Path, Double)] =
  {
    val variable = restrictions.toSparql.dropWhile(_ != '?').drop(1).takeWhile(_ != ' ')

    getAllPaths(endpoint, restrictions, variable)
  }

  private def getAllPaths(endpoint : SparqlEndpoint, restrictions : Restrictions, variable : String) : Traversable[(Path, Double)] =
  {
    val sparql = "SELECT ?p ( count(?" + variable + ") AS ?count ) WHERE {\n" +
      restrictions.toSparql + ".\n" +
      "?" + variable + " ?p ?o\n" +
      "}\n" +
      "GROUP BY ?p\n" +
      "ORDER BY DESC (?count)"

    val results = endpoint.query(sparql, MaxPropertyCount).toList
    if(!results.isEmpty)
    {
      val maxCount = results.head("count").value.toDouble
      for(result <- results if result.contains("p")) yield
      {
        (new Path(variable, ForwardOperator(Uri.fromURI(result("p").value)) :: Nil),
         result("count").value.toDouble / maxCount)
      }
    }
    else
    {
      Traversable.empty
    }
  }

  private def getInstancesPaths(endpoint : SparqlEndpoint, instances : Traversable[String], variable : String) : Traversable[(Path, Double)] =
  {
    logger.info("Searching for relevant properties in " + endpoint)

    val instanceArray = instances.toArray

    //Get all properties
    val properties = instanceArray.flatMap(instance => getInstanceProperties(endpoint, instance, variable))

    //Compute the frequency of each property
    val propertyFrequencies = properties.groupBy(x => x).mapValues(_.size.toDouble / instanceArray.size).toList

    //Choose the relevant properties
    val relevantProperties = propertyFrequencies.filter{ case (uri, frequency) => frequency > MinFrequency }
      .sortWith(_._2 > _._2).take(MaxPropertyCount)

    logger.info("Found " + relevantProperties.size + " relevant properties in " + endpoint)

    relevantProperties
  }

  private def getInstanceProperties(endpoint : SparqlEndpoint, instanceUri : String, variable : String) : Traversable[Path] =
  {
    var sparql = ""
    sparql += "SELECT DISTINCT ?p \n"
    sparql += "WHERE {\n"
    sparql += " <" + instanceUri + "> ?p ?o\n"
    sparql += "}"

    for(result <- endpoint.query(sparql); binding <- result.values)
      yield new Path(variable, ForwardOperator(Uri.fromURI(binding.value)) :: Nil)
  }
}
