package de.fuberlin.wiwiss.silk.instance

import java.io._
import de.fuberlin.wiwiss.silk.util.FileUtils._
import java.util.logging.Logger
import collection.mutable.{SynchronizedMap, HashMap}
import org.apache.commons.collections.map.LRUMap


/**
 * An instance cache, which caches the instances on the local file system.
 */
class FileInstanceCache(instanceSpec : InstanceSpecification, dir : File, clearOnLoading : Boolean = false, maxPartitionSize : Int = 400) extends InstanceCache
{
  //with new blocking, no block count is specified
  //require(blockCount >= 0, "blockCount must be greater than 0 (blockCount=" + blockCount + ")")
  require(maxPartitionSize >= 0, "maxPartitionSize must be greater than 0 (maxPartitionSize=" + maxPartitionSize + ")")

  private val logger = Logger.getLogger(getClass.getName)

  private var blocks = new HashMap[BigInt,Block]() with SynchronizedMap[BigInt, Block]

  private var loadedInstances = new LRUMap(5000)

  private var cacheHits = 0

  private var cacheMisses = 0

  private var blockDirsOnFirstLevel = 10
  private var blockDirsOnSecondLevel = 10
  private var instanceDirsOnFirstLevel = 10
  private var instanceDirsOnSecondLevel = 10
  //private val largestBlock: Tuple2[Int][Block] = (-1,null)


  @volatile private var writing = false

  override def write(instances : Traversable[Instance], blockingFunction : Option[Instance => Set[BigInt]] = None)
  {
    val startTime = System.currentTimeMillis()
    writing = true
    var instanceCount = 0
    var blockCount = 0
    var lastReportedBlockCount = 0
    logger.info("writing " + instances.size + " instances...")
    try
    {
      for(instance <- instances)
      {
        writeInstance(instance)
        val blockIndexes:Set[BigInt] = blockingFunction.map(f => f(instance):Set[BigInt]).getOrElse(Set(0))
        for(blockIndex:BigInt <- blockIndexes)
        {
          //logger.info("handling loaded instance" + instance.uri )
          //if(block < 0 || block >= blockCount) throw new IllegalArgumentException("Invalid blocking function. (Allocated Block: " + block + ")")
          if (!blocks.contains(blockIndex)){
            //logger.info("block " + blockIndex + " is new, creating...")
            blocks.put(blockIndex, new Block(blockIndex))
          }
          //logger.info("writing instance" + instance.uri + " to block " + blockIndex)
          blocks(blockIndex).write(instance)
          blockCount += 1
        }

        if(!blockIndexes.isEmpty) instanceCount += 1
        if ((instanceCount > 0 && instanceCount % 5000 == 0) || (blocks.size > lastReportedBlockCount && blocks.size % 100 == 0)){
          logger.info("loaded " + instanceCount + " instances for '?" + instanceSpec.variable +"', avg blocks per instance:  " + (blockCount / instanceCount) + ", total no of blocks: "+ blocks.size + ", continuing...")
          lastReportedBlockCount = blocks.size
        }
      }
      /*
      actually not needed, the last partition of each block remains in memory

      logger.info("closing open blocks...")
      //iterate over blocks and write last parititions (i.e., close blocks)
      for (block <- blocks.values)
      {
        block.close
      }
      logger.info("all blocks closed")
      */
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
    read(readUris(block, partition))
  }

  override def read(instanceUris: Array[String]) =
  {
    for (uri <- instanceUris) yield
    {
      read(uri)
    }
  }

  override def read(instanceUri: String) =
  {
    readInstance(instanceUri)
  }

  override def readUris(block : BigInt, partition : Int): Array[String] =
  {
    //require(block >= 0 && block < blockCount, "0 <= block < " + blockCount + " (block = " + block + ")")
    require(blocks.contains(block))
    val currentBlock = blocks(block)
    require(partition >= 0 && partition < currentBlock.partitionCount, "0 <= partition < " + currentBlock.partitionCount + " (partition = " + partition + ")")

    currentBlock.read(partition)
  }

  override def blockCount: Int = {
    blocks.size
  }

  override def blockIndices():Seq[BigInt] = {
    blocks.keys.toSeq
  }

  override def partitionCount(block : BigInt) =
  {
    //require(blocks.contains(block))
    //require(block >= 0 && block < blockCount, "0 <= block < " + blockCount + " (block = " + block + ")")
    if (blocks.contains(block))
      blocks(block).partitionCount
    else
      0
  }

  override def clear()
  {
    logger.info("deleting block caches")
    for(block <- blocks.values)
    {
      block.clear()
    }
    logger.info("deleting instance caches")
    val instFile = new File(dir,"inst")
    if (instFile.exists) {
      instFile.deleteRecursive
    }

  }

  override def close()
  {
    logger.info("block closing is disabled - open blocks remain in memory until they are no longer needed")
    //we don't need to close open blocks - they remain in memory
//    for(block <- blocks.values)
//    {
//      block.close()
//    }
  }

  ///

  private def writeInstance(instance:Instance)
  {
    val instanceFile = new File(dir, createInstancePath(instance.uri))
    if (!instanceFile.getParentFile.exists) instanceFile.getParentFile.mkdirs()
    val stream = new DataOutputStream(new FileOutputStream(instanceFile))
    try
    {
        instance.serialize(stream)
    }
    finally
    {
      stream.close()
    }
    //logger.info("Written instance " + instance.uri + " to file " + instanceFile)
  }




  private def readInstance(uri: String): Instance =
  {
    if (cacheHits > 0 && cacheHits % 50000 == 0 || cacheMisses > 0 && cacheMisses % 50000 == 0){
      logger.info("Cache for variable '?" + instanceSpec.variable +"': cacheHits: " + cacheHits + ", cacheMisses: " + cacheMisses)
    }
    if (this.loadedInstances.containsKey(uri)) {
      //TODO: this needs synchronization when used concurrently
      this.cacheHits += 1
      return this.loadedInstances.get(uri).asInstanceOf[Instance]
    } else {
      this.cacheMisses += 1
      var instance: Instance = null
      val instanceFile = new File(dir, createInstancePath(uri))
      val stream = new DataInputStream(new FileInputStream(instanceFile))
      try
      {
          instance = Instance.deserialize(stream, instanceSpec)
      }
      finally
      {
        stream.close()
        //logger.info("read instance " + uri + " from file " + instanceFile)
      }
      this.loadedInstances.put(uri, instance)
      instance
    }
  }

  private def createInstancePath(uri: String): String =
  {
    val hash = math.abs(uri.hashCode())
    "inst/" + (hash % instanceDirsOnFirstLevel) +"/" +((hash/instanceDirsOnFirstLevel) % instanceDirsOnSecondLevel) + "/" + uri.replaceAll("[:/\\.]","-") + ".inst"
  }

  private def createBlockPath(block:BigInt): String =
  {
    "blocks/"+(block % blockDirsOnFirstLevel) +"/" +((block/blockDirsOnFirstLevel) % blockDirsOnSecondLevel) + "/" + block
  }

  private class Block(block : BigInt)
  {
    @volatile var partitionCount = 0

    private val blockDir = new File(dir, createBlockPath(block))

    private val lastPartition = new Array[String](maxPartitionSize)
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

    def read(partition : Int) : Array[String] =
    {
      if(partition == partitionCount - 1)
      {
        val partition = new Array[String](lastPartitionSize)
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

      lastPartition(lastPartitionSize) = instance.uri
      lastPartitionSize += 1

      if(lastPartitionSize == maxPartitionSize)
      {
        writePartitionToFile()
        logger.info("Written partition " + (partitionCount - 1) + " of block " + block + ", " + lastPartitionSize + " instances")
        lastPartitionSize = 0
        partitionCount += 1
      }
    }

    def clear()
    {
      partitionCount = 0
      lastPartitionSize = 0
      //TODO execute deleteRecursive once on whole cache?
      try {
        if (blockDir.exists) {
          blockDir.deleteRecursive()
        }
      } catch {
        case ex: Exception => logger.warning("could not delete directory " + blockDir + ", cause: " + ex.getMessage)
      }
    }

//    disabled - we don't need to close blocks, open blocks remain in memory
//    def close()
//    {
//      if((partitionCount > 1 && lastPartitionSize > 0) || (partitionCount == 1 && lastPartitionSize > 1))
//      {
//        writePartitionToFile()
//      }
//    }

    private def readPartitionFromFile(partition : Int) =
    {
      val stream = new DataInputStream(new FileInputStream(blockDir+"/partition" + partition.toString))

      try
      {
        val partitionSize = stream.readInt()
        val partition = new Array[String](partitionSize)

        for(i <- 0 until partitionSize)
        {
           //partition(i) = Instance.deserialize(stream, instanceSpec)
            //Read uri
            partition(i) = stream.readUTF
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
          stream.writeUTF(lastPartition(i))
        }
      }
      finally
      {
        stream.close()
      }

      //logger.info("Written partition " + (partitionCount - 1) + " of block " + block + ", " + lastPartitionSize + " instances")
    }
  }
}
