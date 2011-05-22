package de.fuberlin.wiwiss.silk.linkspec

import condition._
import input.{Input, TransformInput, Transformer, PathInput}
import de.fuberlin.wiwiss.silk.instance.Path
import xml.Node
import de.fuberlin.wiwiss.silk.output.Output
import de.fuberlin.wiwiss.silk.config.Prefixes
import de.fuberlin.wiwiss.silk.util.{Uri, Identifier, ValidatingXMLReader, SourceTargetPair}
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.linkspec.BlockingMode.{Strict,Lax,Disabled}
import de.fuberlin.wiwiss.silk.output._

/**
 * Represents a Silk Link Specification.
 *
 * @param id The id which identifies this link specification. May only contain alphanumeric characters (a - z, 0 - 9).
 */
case class LinkSpecification(id : Identifier,
                             linkType : Uri,
                             datasets : SourceTargetPair[DatasetSpecification],
                             condition : LinkCondition,
                             filter : LinkFilter,
                             outputs : Traversable[Output])
{
  /**
   * Serializes this Link Specification as XML.
   */
  def toXML(implicit prefixes : Prefixes) : Node =
  {
    <Interlink id={id}>
      <LinkType>{linkType.toTurtle}</LinkType>
      { datasets.source.toXML(true) }
      { datasets.target.toXML(false) }
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

  def load(implicit prefixes : Prefixes) =
  {
    new ValidatingXMLReader(node => fromXML(node), schemaLocation)
  }

  /**
   * Reads a Link Specification from XML.
   */
  def fromXML(node : Node)(implicit prefixes : Prefixes) : LinkSpecification =
  {
    new LinkSpecification(
      node \ "@id" text,
      resolveQualifiedName(node \ "LinkType" text, prefixes),
      new SourceTargetPair(DatasetSpecification.fromXML(node \ "SourceDataset" head),
                           DatasetSpecification.fromXML(node \ "TargetDataset" head)),
      readLinkCondition(node \ "LinkCondition" head),
      LinkFilter.fromXML(node \ "Filter" head),
      (node \ "Outputs" \ "Output").map(Output.fromXML)
    )
  }

  private def readLinkCondition(node : Node)(implicit prefixes : Prefixes) =
  {
    LinkCondition(readOperators(node.child).headOption)
  }

  private def readOperators(nodes : Seq[Node])(implicit prefixes : Prefixes) : Seq[Operator] =
  {
    nodes.collect
    {
      case node @ <Aggregate>{_*}</Aggregate> => readAggregation(node)
      case node @ <Compare>{_*}</Compare> => readComparison(node)
      case node @ <Random>{_*}</Random> => readRandomOperator(node)
      case node @ <Classify>{_*}</Classify> => readClassifierAggregation(node)
    }
  }

  private def readFeatures(nodes : Seq[Node])(implicit prefixes : Prefixes) : Traversable[Feature] =
  {
    nodes.collect
    {
      case node @ <OperatorFeature>{_*}</OperatorFeature> => readOperatorFeature(node)
      case node @ <ExtractorFeature>{_*}</ExtractorFeature> => readExtractorFeature(node)
    }
  }

  private def readFeatureVectorHandlers(nodes : Seq[Node], features: Traversable[Feature])(implicit prefixes : Prefixes) : Traversable[FeatureVectorHandler] =
  {
    nodes.collect
    {
      case node @ <FeatureVectorHandler>{_*}</FeatureVectorHandler> => readFeatureVectorHandler(node, features)
    }
  }

  private def readOutputRows(nodes: Seq[Node], features: Traversable[Feature])(implicit prefixes : Prefixes) : Seq[OutputRow] =
  {
    nodes.collect
        {
          case node @ <OutputRow>{_*}</OutputRow> => new OutputRow(readFields(node.child, features))
        }
  }

  private def readFields(nodes: Seq[Node], features: Traversable[Feature])(implicit prefixes : Prefixes) : Seq[Fields] =
  {
    nodes.collect
        {
          case node @ <FeatureVectorFields >{_*}</FeatureVectorFields> => {
            val omitFieldsStr = node \ "@omitFields" text;
            val selectFieldsStr = node \ "@selectFields" text;
            val missingValueStr = node \ "@missingValue" text;
            new FeatureVectorFields(
              features,
              if (missingValueStr.isEmpty) "?" else  missingValueStr,
              if (omitFieldsStr.isEmpty) Seq.empty else omitFieldsStr.split("[,;\\s]+").toSeq,
              if (selectFieldsStr.isEmpty) Seq.empty else selectFieldsStr.split("[,;\\s]+").toSeq
            )
          }
          case node @ <ClassificationResultField>{_*}</ClassificationResultField> => new ClassificationResultFields(node \ "@name" text)
          case node @ <InputField>{_*}</InputField> => new InputFields(node \ "@name" text, readInputs(node.child))
        }
  }

  /**
   * Only reads the first classifier found.
   */
  private def readClassifier(nodes : Seq[Node])(implicit prefixes : Prefixes) : ClassifierAggregator =
  {
    val classifiers = nodes.collect
    {
      case node @ <Classifier>{_*}</Classifier> => readClassifierAggregator(node)
    }
    if (classifiers.isEmpty){
      throw new IllegalStateException("no <Classifier> element found!")
    } else {
      classifiers.head
    }
  }

  private def readAggregation(node : Node)(implicit prefixes : Prefixes) : Aggregation =
  {
    val requiredStr = node \ "@required" text
    val weightStr = node \ "@weight" text
    val thresholdStr = node \ "@threshold" text
    val aggregator = Aggregator(node \ "@type" text, readParams(node))

    new Aggregation(
      if(requiredStr.isEmpty) false else requiredStr.toBoolean,
      if(weightStr.isEmpty) 1 else weightStr.toInt,
      if(thresholdStr.isEmpty) 0.0 else thresholdStr.toDouble,
      readOperators(node.child),
      aggregator
    )
  }



  private def readClassifierAggregation(node : Node)(implicit prefixes : Prefixes) : ClassifierAggregation =
  {
    val requiredStr = node \ "@required" text
    val weightStr = node \ "@weight" text
    val thresholdStr = node \ "@threshold" text
    val classifierAggregator = readClassifier(node.child)
    val features = readFeatures(node.child)
    val agg = new ClassifierAggregation(
      if(requiredStr.isEmpty) false else requiredStr.toBoolean,
      if(weightStr.isEmpty) 1 else weightStr.toInt,
      if(thresholdStr.isEmpty) 0.0 else thresholdStr.toDouble,
      features,
      classifierAggregator
    )
    agg.setFeatureVectorHandlers(readFeatureVectorHandlers(node.child,features))
    agg
  }

  private def readFeatureVectorHandler(node : Node, features: Traversable[Feature])(implicit prefixes : Prefixes) : FeatureVectorHandler =
  {
    val output = FeatureVectorOutput(node \ "@type" text, readParams(node))
    val outputRows = readOutputRows(node.child, features)
    output.initialize(outputRows(0).getHeader)
    val ignoreThresholdStr = node \ "@ignoreThreshold" text;
    new FeatureVectorHandler(
      outputRows ,
      output,
      if (ignoreThresholdStr.isEmpty) false else ignoreThresholdStr.toBoolean
    )
  }

  private def readClassifierAggregator(node : Node)(implicit prefixes : Prefixes) : ClassifierAggregator =
  {
    val clType = node \ "@type" text
    val params = readParams(node)
    System.out.println("using classifier type " + clType + " with params " + params)
    ClassifierAggregator(clType, params)
  }

  /**
   * Feature only uses the first operator.
   */
  private def readOperatorFeature(node : Node)(implicit prefixes : Prefixes) : OperatorFeature =
  {
    val requiredStr = node \ "@required" text
    val blockingModeStr = node \ "@blockingMode" text
    val nameStr = node \ "@featureName" text
    val datatypeStr:String = node \ "@dataType" text
    val operators = readOperators(node.child)
    logger.info("required="+requiredStr+", featureName="+nameStr+", dataType="+ datatypeStr + ", operators=" + operators)
    new OperatorFeature(
      nameStr,
      if (datatypeStr.isEmpty) "numeric" else datatypeStr,
      if(requiredStr.isEmpty) false else requiredStr.toBoolean,
      if (blockingModeStr.isEmpty) BlockingMode.Strict else BlockingMode.fromString(blockingModeStr),
      readParams(node),
      operators.head)
  }

  private def readExtractorFeature(node : Node)(implicit prefixes : Prefixes) : ExtractorFeature =
  {
    val requiredStr = node \ "@required" text
    val blockingModeStr = node \ "@blockingMode" text
    val nameStr = node \ "@featureName" text
    val datatypeStr:String = node \ "@dataType" text
    val inputs = readInputs(
      node.child)
    val extractor = Extractor(node \ "@extractor" text, readParams(node))
    logger.info("extractor feature created, featureName=" + nameStr)
    new ExtractorFeature(
      nameStr,
      if (datatypeStr.isEmpty) "numeric" else datatypeStr,
      if (requiredStr.isEmpty) false else requiredStr.toBoolean,
      if (blockingModeStr.isEmpty) BlockingMode.Strict else BlockingMode.fromString(blockingModeStr),
      readParams(node), SourceTargetPair(inputs(0),inputs(1)),
      extractor
    )
  }
  
  private def readComparison(node : Node)(implicit prefixes : Prefixes) : Comparison =
  {
    val requiredStr = node \ "@required" text
    val weightStr = node \ "@weight" text
    val thresholdStr = node \ "@threshold" text
    val metric = Metric(node \ "@metric" text, readParams(node))
    val debugLabel:String = node \  "@debugLabel" text
    val inputs = readInputs(node.child)
    logger.info("read comparison: " + metric)
    new Comparison(
      if(requiredStr.isEmpty) false else requiredStr.toBoolean,
      if(weightStr.isEmpty) 1 else weightStr.toInt,
      if(thresholdStr.isEmpty) 0.0 else thresholdStr.toDouble,
      SourceTargetPair(inputs(0), inputs(1)),
      metric,
      if (debugLabel.isEmpty) null else debugLabel
    )
  }

  private def readRandomOperator(node : Node)(implicit prefixes : Prefixes) : RandomOperator =
  {
    val requiredStr = node \ "@required" text
    val weightStr = node \ "@weight" text
    val thresholdStr = node \ "@threshold" text
    val randomGenerator = RandomGenerator(node \ "@generator" text, readParams(node))
    logger.info("read random generator: " + randomGenerator)
    new RandomOperator(
      if(requiredStr.isEmpty) false else requiredStr.toBoolean,
      if(weightStr.isEmpty) 1 else weightStr.toInt,
      if(thresholdStr.isEmpty) 0.0 else thresholdStr.toDouble,
      randomGenerator
    )
  }

  private def readInputs(nodes : Seq[Node])(implicit prefixes : Prefixes) : Seq[Input] =
  {
    nodes.collect {
      case p @ <Input/> =>
      {
        val pathStr = p \ "@path" text
        val path = Path.parse(pathStr)
        PathInput(path)
      }
      case p @ <TransformInput>{_*}</TransformInput> =>
      {
        val transformer = Transformer(p \ "@function" text, readParams(p))
        TransformInput(readInputs(p.child), transformer)
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
