package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import xml.Elem
import de.fuberlin.wiwiss.silk.linkspec.input.{Transformer, TransformInput, PathInput, Input}
import de.fuberlin.wiwiss.silk.config.Prefixes
import BigInt._

/**
 * A Link Condition specifies the conditions which must hold true so that a link is generated between two instances.
 */
case class LinkCondition(rootOperator : Option[Operator])
{
  /**
   * Computes the similarity between two instances.
   *
   * @param instances The instances to be compared.
   * @param threshold The similarity threshold.
   *
   * @return The similarity as a value between 0.0 and 1.0.
   *         Returns 0.0 if the similarity is lower than the threshold.
   *         None, if no similarity could be computed.
   */
  def apply(instances : SourceTargetPair[Instance], threshold : Double) : Double =
  {
    rootOperator match
    {
      case Some(operator) => operator(instances, threshold).getOrElse(0.0)
      case None => 0.0
    }
  }

  /**
   * Indexes an instance.
   *
   * @param instance The instance to be indexed
   * @param threshold The similarity threshold.
   *
   * @return A set of (multidimensional) indexes. Instances within the threshold will always get the same index.
   */
  def index(instance : Instance, threshold : Double, blocks: Int) : Set[Int] =
  {
    val instanceIndices:Set[BigInt] = rootOperator match
    {
      case Some(operator) =>
      {
        val indexes = operator.index(instance, threshold)
        val bigIndexes = indexes.map(_.map(BigInt(_)))

        if (instance.uri == "http://rdf.tripwolf.com/tw.locations/9687"){
          System.out.println("--indices: " + bigIndexes)
          System.out.println("--blocks: " + blocks)
        }
        //Convert the index vectors to scalars
        for(index <- bigIndexes) yield
        {
          (index zip operator.blockCounts).foldLeft(BigInt(0)){case (iLeft:BigInt, (iRight, blocks)) => {
              var blocksToUse = blocks
              if (blocksToUse == 0) blocksToUse=1;
              val res = iLeft * blocksToUse + iRight
              if (instance.uri == "http://rdf.tripwolf.com/tw.locations/9687"){
                System.out.println("blocks: " + blocks + ", iRight: " + iRight + " intermediate result: " + res)
              }
              res
            }
          }
        }:BigInt
      }
      case None => Set.empty
    }
    if (instance.uri == "http://rdf.tripwolf.com/tw.locations/9687"){
      System.out.println("instance indices:" + instanceIndices)
    }
    val blockIndices = for (index <- instanceIndices) yield {
      val blockIndex = (index % blocks).toInt
      if (blockIndex < 0 ) {
        blockIndex + blocks
      } else {
        blockIndex
      }
    }
    if (instance.uri == "http://rdf.tripwolf.com/tw.locations/9687"){
      System.out.println("block indices: " + blockIndices)
    }
    blockIndices
  }

  /**
   * The number of blocks in each dimension of the index.
   */
  val blockCount =
  {
    rootOperator match
    {
      case Some(operator) => operator.blockCounts.foldLeft(1)(_ * _)
      case None => 1
    }
  }

  /**
   * Serializes this Link Condition as XML.
   */
  def toXML(implicit prefixes : Prefixes) =
  {
    <LinkCondition>
      { rootOperator.toList.map(_.toXML) }
    </LinkCondition>
  }
}
