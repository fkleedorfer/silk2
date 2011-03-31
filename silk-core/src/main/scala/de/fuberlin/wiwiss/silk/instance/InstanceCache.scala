package de.fuberlin.wiwiss.silk.instance

import xml.Node

/**
 * A cache of instances.
 */
trait InstanceCache
{
  /**
   * Writes to this cache.
   */
  def write(instances : Traversable[Instance], blockingFunction : Option[Instance => Set[BigInt]] = None) : Unit

  def isWriting : Boolean

  /**
   * Reads a partition of a block.
   */
  def read(block : BigInt, partition : Int) : Array[Instance]

  /**
   * Removes all instances from this cache.
   */
  def clear()

  def close()

  /**
   *  The number of blocks in this cache.
   */
  val blockCount : Int

  /**
   * The block indices of all blocks.
   */
  def blockIndices(): Seq[BigInt]

  /**
   * The number of partitions in a specific block.
   */
  def partitionCount(block : BigInt) : Int

  /**
   * Serializes the complete Cache as XML
   */
  def toXML =
  {
    <InstanceCache>
      <TODO message="xml serialization not implemented" />

    {
//      for(block <- 0 until blockCount) yield
//      {
//        <Block id={block.toString}>
//        {
//          for(partition <- 0 until partitionCount(block)) yield
//          {
//            <Partition>
//            {
//              for(instance <- read(block, partition)) yield instance.toXML
//            }
//            </Partition>
//          }
//        }
//        </Block>
//      }
    }
    </InstanceCache>
  }

  /**
   * Reads instances from XML
   */
  def fromXML(node : Node, instanceSpec : InstanceSpecification)
  {
//    val instances = new Traversable[Instance]
//    {
//      var currentBlock = 0
//
//      override def foreach[U](f : Instance => U)
//      {
//        for(blockNode <- node \ "Block")
//        {
//          currentBlock = (blockNode \ "@id" text).toInt
//
//          for(partitionNode <- blockNode \ "Partition";
//              instanceNode <- partitionNode \ "_")
//          {
//            f(Instance.fromXML(instanceNode, instanceSpec))
//          }
//        }
//      }
//    }
//
//    write(instances, Some(_ => Set(instances.currentBlock)))
      throw new UnsupportedOperationException("not implemented")
  }
}