package de.fuberlin.wiwiss.silk.linkspec

import condition._
import input.{Input, TransformInput, Transformer, PathInput}
import de.fuberlin.wiwiss.silk.instance.Path
import xml.Node
import de.fuberlin.wiwiss.silk.output.Output
import de.fuberlin.wiwiss.silk.util.{Identifier, ValidatingXMLReader, SourceTargetPair}
import java.util.logging.Logger

/**
 * @param id The id which identifies this link specification. May only contain alphanumeric characters (a - z, 0 - 9).
 */
case class LinkSpecification(id : Identifier,
                             linkType : String,
                             datasets : SourceTargetPair[DatasetSpecification],
                             condition : LinkCondition,
                             filter : LinkFilter,
                             outputs : Traversable[Output])
{


  def toXML : Node =
  {
    <Interlink id={id}>
      <LinkType>{"<" + linkType + ">"}</LinkType>

      <SourceDataset dataSource={datasets.source.sourceId} var={datasets.source.variable}>
        <RestrictTo>{datasets.source.restriction}</RestrictTo>
      </SourceDataset>

      <TargetDataset dataSource={datasets.target.sourceId} var={datasets.target.variable}>
        <RestrictTo>{datasets.target.restriction}</RestrictTo>
      </TargetDataset>

      { condition.toXML }

      { filter.toXML }

      <Outputs>
        { outputs.map(_.toXML) }
      </Outputs>
    </Interlink>
  }
}

object LinkSpecification
{
  private val schemaLocation = "de/fuberlin/wiwiss/silk/linkspec/LinkSpecificationLanguage.xsd"
  private val logger = Logger.getLogger(classOf[LinkSpecification].getName)

  def load(prefixes : Map[String, String]) =
  {
    new ValidatingXMLReader(node => fromXML(node, prefixes), schemaLocation)
  }

  def fromXML(node : Node, prefixes : Map[String, String]) : LinkSpecification =
  {
    new LinkSpecification(
      node \ "@id" text,
      resolveQualifiedName(node \ "LinkType" text, prefixes),
      new SourceTargetPair(DatasetSpecification.fromXML(node \ "SourceDataset" head),
                           DatasetSpecification.fromXML(node \ "TargetDataset" head)),
      readLinkCondition(node \ "LinkCondition" head, prefixes),
      LinkFilter.fromXML(node \ "Filter" head),
      (node \ "Outputs" \ "Output").map(Output.fromXML)
    )
  }

  private def readLinkCondition(node : Node, prefixes : Map[String, String]) =
  {
    LinkCondition(readOperators(node.child, prefixes).headOption)
  }

  private def readOperators(nodes : Seq[Node], prefixes : Map[String, String]) : Traversable[Operator] =
  {
    nodes.collect
    {
      case node @ <Aggregate>{_*}</Aggregate> => readAggregation(node, prefixes)
      case node @ <Compare>{_*}</Compare> => readComparison(node, prefixes)
      case node @ <Classify>{_*}</Classify> => readClassifierAggregation(node,prefixes)
    }
  }

  private def readFeatures(nodes : Seq[Node], prefixes : Map[String, String]) : Traversable[Feature] =
  {
    nodes.collect
    {
      case node @ <OperatorFeature>{_*}</OperatorFeature> => readOperatorFeature(node, prefixes)
      case node @ <ExtractorFeature>{_*}</ExtractorFeature> => readExtractorFeature(node, prefixes)
    }
  }

  private def readFeatureVectorHandlers(nodes : Seq[Node], prefixes : Map[String, String]) : Traversable[FeatureVectorHandler] =
  {
    nodes.collect
    {
      case node @ <FeatureVectorHandler>{_*}</FeatureVectorHandler> => readFeatureVectorHandler(node, prefixes)
    }
  }

  /**
   * Only reads the first classifier found.
   */
  private def readClassifier(nodes : Seq[Node], prefixes : Map[String, String]) : ClassifierAggregator =
  {
    val classifiers = nodes.collect
    {
      case node @ <Classifier>{_*}</Classifier> => readClassifierAggregator(node, prefixes)
    }
    if (classifiers.isEmpty){
      throw new IllegalStateException("no <Classifier> element found!")
    } else {
      classifiers.head
    }
  }

  private def readAggregation(node : Node, prefixes : Map[String, String]) : Aggregation =
  {
    val requiredStr = node \ "@required" text
    val weightStr = node \ "@weight" text
    val thresholdStr = node \ "@threshold" text
    val aggregator = Aggregator(node \ "@type" text, readParams(node))

    new Aggregation(
      if(requiredStr.isEmpty) false else requiredStr.toBoolean,
      if(weightStr.isEmpty) 1 else weightStr.toInt,
      if(thresholdStr.isEmpty) 0.0 else thresholdStr.toDouble,
      readOperators(node.child, prefixes),
      aggregator
    )
  }



  private def readClassifierAggregation(node : Node, prefixes : Map[String, String]) : ClassifierAggregation =
  {
    val requiredStr = node \ "@required" text
    val weightStr = node \ "@weight" text
    val thresholdStr = node \ "@threshold" text
    val classifierAggregator = readClassifier(node.child, prefixes)

    val agg = new ClassifierAggregation(
      if(requiredStr.isEmpty) false else requiredStr.toBoolean,
      if(weightStr.isEmpty) 1 else weightStr.toInt,
      if(thresholdStr.isEmpty) 0.0 else thresholdStr.toDouble,
      readFeatures(node.child, prefixes),
      classifierAggregator
    )
    agg.asInstanceOf[FeatureVectorEmitter].setFeatureVectorHandlers(readFeatureVectorHandlers(node.child,prefixes))
    agg
  }

  private def readFeatureVectorHandler(node : Node, prefixes : Map[String, String]) : FeatureVectorHandler =
  {
    FeatureVectorHandler(node \ "@type" text, readParams(node))
  }

  private def readClassifierAggregator(node : Node, prefixes : Map[String, String]) : ClassifierAggregator =
  {
    val clType = node \ "@type" text
    val params = readParams(node)
    System.out.println("using classifier type " + clType + " with params " + params)
    ClassifierAggregator(clType, params)
  }

  /**
   * Feature only uses the first operator.
   */
  private def readOperatorFeature(node : Node, prefixes : Map[String, String]) : OperatorFeature =
  {
    val requiredStr = node \ "@required" text
    val nameStr = node \ "@featureName" text
    val datatypeStr:String = node \ "@dataType" text
    val operators = readOperators(node.child,prefixes)
    logger.info("required="+requiredStr+", featureName="+nameStr+", dataType="+ datatypeStr + ", operators=" + operators)
    new OperatorFeature(
      nameStr,
      if (datatypeStr.isEmpty) "numeric" else datatypeStr,
      if(requiredStr.isEmpty) false else requiredStr.toBoolean,
      readParams(node),
      operators.head)
  }

  private def readExtractorFeature(node : Node, prefixes : Map[String, String]) : ExtractorFeature =
  {
    val requiredStr = node \ "@required" text
    val nameStr = node \ "@featureName" text
    val datatypeStr:String = node \ "@dataType" text
    val inputs = readInputs(
      node.child, prefixes)
    logger.info("extractor feature created, featureName=" + nameStr)
    new ExtractorFeature(
      nameStr,
      if (datatypeStr.isEmpty) "numeric" else datatypeStr,
      if (requiredStr.isEmpty) false else requiredStr.toBoolean,
      readParams(node), SourceTargetPair(inputs(0),inputs(1)))
  }
  
  private def readComparison(node : Node, prefixes : Map[String, String]) : Comparison =
  {
    val requiredStr = node \ "@required" text
    val weightStr = node \ "@weight" text
    val thresholdStr = node \ "@threshold" text
    val metric = Metric(node \ "@metric" text, readParams(node))
    val inputs = readInputs(node.child, prefixes)
    logger.info("read comparison: " + metric)
    new Comparison(
      if(requiredStr.isEmpty) false else requiredStr.toBoolean,
      if(weightStr.isEmpty) 1 else weightStr.toInt,
      if(thresholdStr.isEmpty) 0.0 else thresholdStr.toDouble,
      SourceTargetPair(inputs(0), inputs(1)),
      metric
    )
  }

  private def readInputs(nodes : Seq[Node], prefixes : Map[String, String]) : Seq[Input] =
  {
    nodes.collect {
      case p @ <Input/> =>
      {
        val pathStr = p \ "@path" text
        val path = Path.parse(pathStr, prefixes)
        PathInput(path)
      }
      case p @ <TransformInput>{_*}</TransformInput> =>
      {
        val transformer = Transformer(p \ "@function" text, readParams(p))
        TransformInput(readInputs(p.child, prefixes), transformer)
      }
    }
  }

  private def readParams(element : Node) : Map[String, String] =
  {
    element \ "Param" map(p => (p \ "@name" text, p \ "@value" text)) toMap
  }

  private def resolveQualifiedName(name : String, prefixes : Map[String, String]) =
  {
    if(name.startsWith("<") && name.endsWith(">"))
    {
      name.substring(1, name.length - 1)
    }
    else
    {
      name.split(":", 2) match
      {
        case Array(prefix, suffix) => prefixes.get(prefix) match
        {
          case Some(resolvedPrefix) => resolvedPrefix + suffix
          case None => throw new IllegalArgumentException("Unknown prefix: " + prefix)
        }
        case _ => throw new IllegalArgumentException("No prefix found in " + name)
      }
    }
  }
}
