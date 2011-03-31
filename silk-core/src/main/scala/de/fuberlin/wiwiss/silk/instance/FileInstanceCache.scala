package de.fuberlin.wiwiss.silk.instance

import java.io._
import de.fuberlin.wiwiss.silk.util.FileUtils._
import java.util.logging.Logger
import collection.mutable.HashMap


/**
 * An instance cache, which caches the instances on the local file system.
 */
class FileInstanceCache(instanceSpec : InstanceSpecification, dir : File, clearOnLoading : Boolean = false, maxPartitionSize : Int = 1000) extends InstanceCache
{
  //with new blocking, no block count is specified
  //require(blockCount >= 0, "blockCount must be greater than 0 (blockCount=" + blockCount + ")")
  require(maxPartitionSize >= 0, "maxPartitionSize must be greater than 0 (maxPartitionSize=" + maxPartitionSize + ")")

  private val logger = Logger.getLogger(getClass.getName)

  private val blocks = new HashMap[BigInt,Block]()

  @volatile private var writing = false

  override def write(instances : Traversable[Instance], blockingFunction : Option[Instance => Set[BigInt]] = None)
  {
    val startTime = System.currentTimeMillis()
    writing = true
    var instanceCount = 0

    try
    {
      for(instance <- instances)
      {
        val blockIndexes:Set[BigInt] = blockingFunction.map(f => f(instance):Set[BigInt]).getOrElse(Set(0))
        for(blockIndex:BigInt <- blockIndexes)
        {
          //if(block < 0 || block >= blockCount) throw new IllegalArgumentException("Invalid blocking function. (Allocated Block: " + block + ")")
          if (!blocks.contains(blockIndex)){
            blocks.updated(blockIndex, new Block(blockIndex))
          }

          blocks(blockIndex).write(instance)
        }

        if(!blockIndexes.isEmpty) instanceCount += 1
      }

      val time = ((System.currentTimeMillis - startTime) / 1000.0)
      logger.info("Finished writing " + instanceCount + " instances with type '" + instanceSpec.restrictions + "' in " + time + " seconds")
    }
    finally
    {
      writing = false
    }
  }

  override def isWriting = writing

  override def read(block : BigInt, partition : Int) =
  {
    //require(block >= 0 && block < blockCount, "0 <= block < " + blockCount + " (block = " + block + ")")
    require(blocks.contains(block))
    val currentBlock = blocks(block)
    require(partition >= 0 && partition < currentBlock.partitionCount, "0 <= partition < " + currentBlock.partitionCount + " (partition = " + partition + ")")

    currentBlock.read(partition)
  }

  override def blockIndices():Seq[BigInt] = {
    blocks.keys.toSeq
  }

  override def partitionCount(block : BigInt) =
  {
    require(blocks.contains(block))
    //require(block >= 0 && block < blockCount, "0 <= block < " + blockCount + " (block = " + block + ")")

    blocks(block).partitionCount
  }

  override def clear()
  {
    for(block <- blocks.values)
    {
      block.clear()
    }
  }

  override def close()
  {
    for(block <- blocks.values)
    {
      block.close()
    }
  }

  private class Block(block : BigInt)
  {
    @volatile var partitionCount = 0

    private val blockDir = dir + "/block" + block.toString + "/"

    private val lastPartition = new Array[Instance](maxPartitionSize)
    @volatile private var lastPartitionSize = 0

    if(clearOnLoading)
      clear()
    else
      load()

    private def load()
    {
      //Retrieve the number of existing partitions
      partitionCount =
      {
        if(blockDir.exists)
        {
          val partitionFiles = blockDir.list.filter(_.startsWith("partition")).map(name => name.dropWhile(!_.isDigit)).filter(!_.isEmpty)

          if(partitionFiles.isEmpty) 0
          else partitionFiles.map(_.toInt).max + 1
        }
        else
        {
          0
        }
      }

      //Load the last partition in memory
      if(partitionCount > 0)
      {
        val readPartition = readPartitionFromFile(partitionCount - 1)
        Array.copy(readPartition, 0, lastPartition, 0, readPartition.size)
        lastPartitionSize = readPartition.size
      }
    }

    def read(partition : Int) : Array[Instance] =
    {
      if(partition == partitionCount - 1)
      {
        val partition = new Array[Instance](lastPartitionSize)
        Array.copy(lastPartition, 0, partition, 0, lastPartitionSize)
        partition
      }
      else
      {
        readPartitionFromFile(partition)
      }
    }

    def write(instance : Instance)
    {
      if(partitionCount == 0) partitionCount = 1

      lastPartition(lastPartitionSize) = instance
      lastPartitionSize += 1

      if(lastPartitionSize == maxPartitionSize)
      {
        writePartitionToFile()
        lastPartitionSize = 0
        partitionCount += 1
      }
    }

    def clear()
    {
      partitionCount = 0
      lastPartitionSize = 0
      //TODO execute deleteRecursive once on whole cache?
      blockDir.deleteRecursive()
    }

    def close()
    {
      if(lastPartitionSize > 0)
      {
        writePartitionToFile()
      }
    }

    private def readPartitionFromFile(partition : Int) =
    {
      val stream = new DataInputStream(new FileInputStream(blockDir + "/partition" + partition.toString))

      try
      {
        val partitionSize = stream.readInt()
        val partition = new Array[Instance](partitionSize)

        for(i <- 0 until partitionSize)
        {
          partition(i) = Instance.deserialize(stream, instanceSpec)
        }

        partition
      }
      finally
      {
        stream.close()
      }
    }

    private def writePartitionToFile()
    {
      if(partitionCount == 1) blockDir.mkdirs()

      val stream = new DataOutputStream(new FileOutputStream(blockDir + "/partition" + (partitionCount - 1).toString))

      try
      {
        stream.writeInt(lastPartitionSize)
        for(i <- 0 until lastPartitionSize)
        {
          lastPartition(i).serialize(stream)
        }
      }
      finally
      {
        stream.close()
      }

      logger.info("Written partition " + (partitionCount - 1) + " of block " + block)
    }
  }
}
