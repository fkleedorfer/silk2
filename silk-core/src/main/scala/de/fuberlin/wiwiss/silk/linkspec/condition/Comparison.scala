package de.fuberlin.wiwiss.silk.linkspec

import de.fuberlin.wiwiss.silk.instance.Instance
import input.Input
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import java.util.logging.Logger

case class Comparison(required : Boolean, weight : Int, threshold: Double, inputs : SourceTargetPair[Input], metric : Metric) extends Operator
{
  private val logger = Logger.getLogger(classOf[Comparison].getName)
  override def apply(instances : SourceTargetPair[Instance], threshold : Double) : Option[Double] =
  {
    val set1 = inputs.source(instances)
    val set2 = inputs.target(instances)
    
    logger.fine("comparison.apply: set1=" + set1 + ", set2=" + set2)
    
    if(!set1.isEmpty && !set2.isEmpty)
    {
      val similarities = for (str1 <- set1; str2 <- set2) yield metric.evaluate(str1, str2, threshold)
      val maxSim = similarities.max
      if (maxSim >= this.threshold ){
        Some(maxSim)
      } else {
        None
      }
    }
    else
    {
      None
    }
  }

  override def index(instance : Instance, threshold : Double) : Set[Seq[Int]] =
  {
    
    val values = inputs.source(SourceTargetPair(instance, instance)) ++ inputs.target(SourceTargetPair(instance, instance))
    values.flatMap(value => metric.index(value, threshold)).toSet
  }

  override val blockCounts = metric.blockCounts

  override def toString = metric match
  {
    case Metric(name, params) => "Comparison(required=" + required + ", weight=" + weight + ", type=" + name + ", params=" + params + ", inputs=" + inputs + ")"
  }
}
