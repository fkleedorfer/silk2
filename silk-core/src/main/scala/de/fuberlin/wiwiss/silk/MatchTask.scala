package de.fuberlin.wiwiss.silk

import instance.{Instance, InstanceCache}
import linkspec.LinkSpecification
import java.util.logging.{Level, Logger}
import output.Link
import java.util.concurrent._
import scala.math.max
import java.io._
import io.Source
import util.{SynchronizingMultiFileIterator, SourceTargetPair, Task}
import scala.util.Sorting
import collection.immutable._
import collection.mutable.{Buffer, ArrayBuffer, SynchronizedBuffer, ListBuffer, HashMap}


/**
 * Executes the matching.
 * Generates links between the instances according to the link specification.
 */
class MatchTask(linkSpec : LinkSpecification,
                caches : SourceTargetPair[InstanceCache],
                dir : File,
                numThreads : Int) extends Task[Buffer[Link]]
{
  taskName = "Matching"

  private val logger = Logger.getLogger(classOf[MatchTask].getName)

  private val linkBuffer = new ArrayBuffer[Link]() with SynchronizedBuffer[Link]

  /* Enable indexing if blocking is enabled */
  private val indexingEnabled = caches.source.blockCount > 1 || caches.target.blockCount > 1

  @volatile private var cancelled = false

  def links : Buffer[Link] with SynchronizedBuffer[Link] = linkBuffer


  private val pairOrdering = new Ordering[SourceTargetPair[String]]
      {
        override def compare(x:SourceTargetPair[String], y:SourceTargetPair[String]) =
        {
//          var smallerX = x.source
//          var biggerX = x.target
//          var smallerY = y.source
//          var biggerY = y.target
//
//          if (smallerX.compare(biggerX) > 0){
//            var dummy = smallerX
//            smallerX = biggerX
//            biggerX = dummy
//          }
//
//          if (smallerY.compare(biggerY) > 0){
//            var dummy = smallerY
//            smallerY = biggerY
//            biggerY = dummy
//          }
//          val cmp = smallerX.compare(smallerY)
          val cmp = x.source.compare(y.source)
          if (cmp == 0){
            x.target.compare(y.target)
          } else {
            cmp
          }
        }
      }
    private val multiFileIterator: SynchronizingMultiFileIterator[SourceTargetPair[String], BufferedReader]
      = new SynchronizingMultiFileIterator(reader =>
      {
        val line = reader.readLine
        if (line != null){
          val tokens = line.split(" ")
          Some(new SourceTargetPair(tokens(0), tokens(1)))
        } else {
          None
        }
      },
      file => { new BufferedReader(new FileReader(file)) },
      pairOrdering
    )


  /**
   * Executes the matching.
   */
  override def execute() : Buffer[Link] =
  {
    //block counts can differ, only blocks with identical indices are used
    //require(caches.source.blockCount == caches.target.blockCount, "sourceCache.blockCount == targetCache.blockCount")

    //Reset properties
    linkBuffer.clear()
    cancelled = false
    dir.mkdirs


    //Create execution service for the matching tasks
    val startTime = System.currentTimeMillis()
    val executorService = Executors.newFixedThreadPool(numThreads)
    val executor = new ExecutorCompletionService[Traversable[Link]](executorService)

    //Start matching thread scheduler
    val scheduler = new SchedulerThread(executor)
    scheduler.start()
    multiFileIterator.start()
    //executor.submit(new MultiFileIteratorMatcher(this.multiFileIterator, scheduler))
    var matcher = new MultiFileIteratorMatcher(this.multiFileIterator, scheduler)
    matcher.start
    matcher.join

//    //Process finished tasks
//    while(!cancelled && (scheduler.isAlive || multiFileIterator.hasMoreData || matcher.isAlive))
//    {
//      //logger.info("checking executor result")
//      //val result = executor.poll(100, TimeUnit.MILLISECONDS)
//      val result = executor.poll(5000, TimeUnit.MILLISECONDS)
//      //logger.info("cancelled: " + cancelled + ", scheduler.isAlive:" +scheduler.isAlive + ", muFiIt.hasMoreData:" + multiFileIterator.hasMoreData +", matcher.isAlive: " + matcher.isAlive)
//      if(result != null)
//      {
//        linkBuffer.appendAll(result.get)
//
//        //Update status
//        val statusPrefix = if(scheduler.isAlive) "Matching (Still loading):" else "Matching:"
//        val statusLinks = " " + linkBuffer.size + " links generated, "
//        val statusComparisons = " " + MatchTask.comparisonCount + " comparisons performed ("
//        val statusDuplicates = " " + multiFileIterator.skippedDuplicates + " duplicate comparisons skipped)"
//        updateStatus(statusPrefix + statusLinks + statusComparisons + statusDuplicates)
//      }
//    }


    linkBuffer.appendAll(matcher.getLinks)

    //Update status
    val statusPrefix = if(scheduler.isAlive) "Matching (Still loading):" else "Matching:"
    val statusLinks = " " + linkBuffer.size + " links generated, "
    val statusComparisons = " " + MatchTask.comparisonCount + " comparisons performed ("
    val statusDuplicates = " " + multiFileIterator.skippedDuplicates + " duplicate comparisons skipped)"
    updateStatus(statusPrefix + statusLinks + statusComparisons + statusDuplicates)

    logger.info("FINISHING!!")
    multiFileIterator.stop()

    //Shutdown
    if(scheduler.isAlive())
    {
      scheduler.interrupt()
    }
    if(cancelled)
    {
      executorService.shutdownNow()
    }
    else
    {
      executorService.shutdown()
    }

    //Log result
    val time = ((System.currentTimeMillis - startTime) / 1000.0) + " seconds"
    if(cancelled)
    {
      logger.info("Matching cancelled after " + time)
    }
    else
    {
      logger.info("Executed matching in " +  time)
    }

    linkBuffer
  }

  override def stopExecution()
  {
    cancelled = true
  }

  /**
   * Monitors the instance caches and schedules new matching threads whenever a new partition has been loaded.
   */
  private class SchedulerThread(executor : CompletionService[Traversable[Link]]) extends Thread
  {
    private val MIN_PAIR_COUNT_PER_FILE: Int = 5000000
    @volatile var taskCount = 0

    private var sourcePartitions:HashMap[BigInt,Int] = new HashMap[BigInt,Int]()
    private var targetPartitions:HashMap[BigInt,Int] = new HashMap[BigInt,Int]()


    val comparisonPairs = new ListBuffer[SourceTargetPair[String]]()

    override def run()
    {
      try
      {
        while(true)
        {
          //logger.info("next loop in Scheduler")
          val sourceLoaded = !caches.source.isWriting
          val targetLoaded = !caches.target.isWriting

          updateSourcePartitions(sourceLoaded)
          updateTargetPartitions(targetLoaded)

          //logger.info("updated source partitions and target partitions")

          if(sourceLoaded && targetLoaded)
          {
            writePairs(true)
            multiFileIterator.setStopWhenAllProcessed(true)
            logger.info("finished creating pairs")
            return
          }

          Thread.sleep(1000)
        }
      }
      catch
      {
        case ex : InterruptedException =>
      }
    }

    private def updateSourcePartitions(includeLastPartitions : Boolean)
    {
      val newSourcePartitions =
      {
        for(block <- caches.source.blockIndices) yield
        {
          ( block,
            if(includeLastPartitions)
          {
            caches.source.partitionCount(block)
          }
          else
          {
            max(0, caches.source.partitionCount(block) - 1)
          })
        }
      }.toMap


      //create new matchers for new partitions
      //walk over block indices (non-continuous list of BigInt)
      for(block <- newSourcePartitions.keys.toSeq.intersect(caches.target.blockIndices);
          sourcePartition <- (if (sourcePartitions.contains(block)) sourcePartitions(block) else 0) until newSourcePartitions(block);
          targetPartition <- 0 until (if (targetPartitions.contains(block)) targetPartitions(block) else 0))
      {
        //newMatcher(block, sourcePartition, targetPartition)
        //create pairs
        createPairs(block, sourcePartition, targetPartition)
      }
      for ( (key, value) <- newSourcePartitions)
      {
        sourcePartitions.put(key,value)
      }
    }

    private def updateTargetPartitions(includeLastPartitions : Boolean)
    {
      val newTargetPartitions =
      {
        for(block <- caches.target.blockIndices) yield
        {
          (block,
          if(includeLastPartitions)
          {
            caches.target.partitionCount(block)
          }
          else
          {
            max(0, caches.target.partitionCount(block) - 1)
          })
        }
      }.toMap

      val comparisonPairs = new HashSet[SourceTargetPair[String]]()
      //walk over source block indices. If the a block index is also found in the target, start a matcher for the partitions not yet matched
      for(block <- newTargetPartitions.keys.toSeq.intersect(caches.source.blockIndices);
          targetPartition <- (if (targetPartitions.contains(block)) targetPartitions(block) else 0) until newTargetPartitions(block);
          sourcePartition <- 0 until (if (sourcePartitions.contains(block)) sourcePartitions(block) else 0))
      {
        //newMatcher(block, sourcePartition, targetPartition)
        //create pairs
        createPairs(block, sourcePartition, targetPartition)
      }



      for ( (key, value) <- newTargetPartitions)
      {
        targetPartitions.put(key,value)
      }
    }

    private def createPairs(block: BigInt, sourcePartition: Int, targetPartition: Int) {
      //logger.info("creating pairs for block " + block + ", source partition:" + sourcePartition + ", target partition: " + targetPartition)
      //create pairs
      val sourceUris = caches.source.readUris(block, sourcePartition)
      val targetUris = caches.target.readUris(block, targetPartition)
      for (sourceUri <- sourceUris; targetUri <- targetUris)
      {
        if (!sourceUri.equals(targetUri)) {
          comparisonPairs.append(SourceTargetPair(sourceUri, targetUri))
        }
      }
      //write comparison pairs to file
      writePairs()
    }

    private def writePairs(force: Boolean = false)
    {
      //logger.info("writePairs called, force=" + force + ", comparisonPairs.size=" + comparisonPairs.size)
      if (comparisonPairs.size > MIN_PAIR_COUNT_PER_FILE || ( comparisonPairs.size > 0 && force) )
      {
        var index = -1
        MatchTask.lock.synchronized
        {
          index = MatchTask.pairFileIndex
          MatchTask.pairFileIndex += 1
        }
        val outfile = new File(dir, "cmp-" + index + ".pair")
        val out = new PrintWriter(new FileOutputStream(outfile));
        var skippedDuplicates = 0
        try
        {
          var lastPair:SourceTargetPair[String] = null
          for (pair:SourceTargetPair[String] <- Sorting.stableSort(comparisonPairs,pairOrdering.compare(_:SourceTargetPair[String],_:SourceTargetPair[String]) < 0))
          {
            if (lastPair == null || pairOrdering.compare(lastPair,pair) != 0) {
              out.println(pair.source + " " + pair.target)
            } else {
              skippedDuplicates += 1
            }
            lastPair = pair
          }
        }
        finally
        {
          out.close
        }
        logger.info("wrote " + comparisonPairs.size + " pairs to " + outfile + ", skipping " + skippedDuplicates + " duplicates")
        comparisonPairs.clear
        //executor.submit(new PairMatcher(outfile))
        multiFileIterator.addFile(outfile)
        taskCount +=1
      }
    }

    private def newMatcher(block : BigInt, sourcePartition : Int, targetPartition : Int)
    {

//      executor.submit(new Matcher(block, sourcePartition, targetPartition))
//      taskCount += 1
    }
  }





  private class MultiFileIteratorMatcher(multiFileIterator: SynchronizingMultiFileIterator[SourceTargetPair[String], BufferedReader], scheduler: SchedulerThread) extends Thread
  {
    var links = List[Link]()

    override def run() =
    {
      try
      {
        logger.info("starting MultiFileIteratorMatcher task on multi file iterator " )
        //read comparisons to perform from file
        val loadedInstances = new HashMap[String, Instance]()

        //perform comparisons
        var continueReading = true;
        var sourceInstance: Instance = null
        var targetInstance: Instance = null
        while(!cancelled && continueReading )
        {
          //logger.info("checking multi file iterator")
          val currentPair = multiFileIterator.read
          //if None is read, it's the signal to stop processing
          continueReading = currentPair.isDefined
          if (continueReading){

            //logger.info("read pair: " + currentPair)
            val sourceUri = currentPair.get.source
            val targetUri = currentPair.get.target
            try {
              if (sourceInstance == null || ! sourceInstance.uri.equals(sourceUri)) {
                sourceInstance = caches.source.read(sourceUri)
              }
              if (targetInstance == null || ! targetInstance.uri.equals(targetUri)) {
                targetInstance = caches.target.read(targetUri)
              }
              val confidence = linkSpec.condition(SourceTargetPair(sourceInstance, targetInstance), linkSpec.filter.threshold)
              MatchTask.comparisonCount = MatchTask.comparisonCount + 1

              if(confidence >= linkSpec.filter.threshold)
              {
                //logger.info("confidence " +confidence + " computed for " + sourceUri + " , " + targetUri);
                links ::= new Link(sourceInstance.uri, targetInstance.uri, confidence)
              }
              if (MatchTask.comparisonCount % 50000 == 0){
                logger.info("performed " + MatchTask.comparisonCount + " comparisons, skipped " + multiFileIterator.skippedDuplicates + " duplicate comparisons ")
                multiFileIterator.printStats
              }
            } catch {
              case ex: Exception => {
                  logger.severe("error comparing " + sourceUri + " and " + targetUri + ", exception=" + ex)
              }
            }
          }
        }
        logger.info("cleaning source cache folder")
        caches.source.clear
        logger.info("cleaning target cache folder")
        caches.target.clear
        logger.info("finished MultiFileIteratorMatcher task ")
      }
      catch
      {
        case ex : Exception => logger.log(Level.WARNING, "Could not execute MultiFileIteratorMatcher task", ex)
      }
    }

    def getLinks(): Traversable[Link] =
    {
      links
    }

  }


  /**
   * Matches the instances of two partitions.
   */
  private class Matcher(blockIndex : BigInt, sourcePartitionIndex : Int, targetPartitionIndex : Int) extends Callable[Traversable[Link]]
  {
    override def call() : Traversable[Link] =
    {
      var links = List[Link]()

      try
      {
        val sourceInstances = caches.source.read(blockIndex, sourcePartitionIndex)
        val targetInstances = caches.target.read(blockIndex, targetPartitionIndex)

        val sourceIndexes = builtIndex(sourceInstances)
        val targetIndexes = builtIndex(targetInstances)

        for(s <- 0 until sourceInstances.size;
            t <- 0 until targetInstances.size;
            if !indexingEnabled || compareIndexes(sourceIndexes(s), targetIndexes(t)))
        {
          val sourceInstance = sourceInstances(s)
          val targetInstance = targetInstances(t)

          val confidence = linkSpec.condition(SourceTargetPair(sourceInstance, targetInstance), linkSpec.filter.threshold)
          MatchTask.comparisonCount = MatchTask.comparisonCount + 1

          if(confidence >= linkSpec.filter.threshold)
          {
            links ::= new Link(sourceInstance.uri, targetInstance.uri, confidence)
          }
        }
      }
      catch
      {
        case ex : Exception => logger.log(Level.WARNING, "Could not execute match task", ex)
      }

      links
    }

    def builtIndex(instances : Array[Instance]) : Array[Set[BigInt]] =
    {
      if(indexingEnabled)
      {
        instances.map(instance => HashSet(linkSpec.condition.index(instance, linkSpec.filter.threshold).toSeq : _*))
      }
      else
      {
         Array.empty
      }
    }

    def compareIndexes(index1 : Set[BigInt], index2 : Set[BigInt]) =
    {
      index1.exists(index2.contains(_))
    }

    def evaluateCondition(instances : SourceTargetPair[Instance]) =
    {
      linkSpec.condition(instances, linkSpec.filter.threshold)
    }
  }
}

object MatchTask {
  private var comparisonCount = 0
  //TODO: counting pair files this way is not thread-safe... refactor!
  private var pairFileIndex= 0
  var lock : AnyRef = new Object()
}


/**
   * Matches the instances of two partitions.
   */
//  private class PairGenerator(blockIndex : BigInt, sourcePartitionIndex : Int, targetPartitionIndex : Int) extends Callable[Traversable[SourceTargetPair[String]]]
//  {
//    override def call() : Traversable[Link] =
//    {
//      var links = List[SourceTargetPair[String]]()
//
//      try
//      {
//        val sourceInstanceUris = caches.source.readUris(blockIndex, sourcePartitionIndex)
//        val targetInstanceUris = caches.target.readUris(blockIndex, targetPartitionIndex)
//
//
//        for(s <- 0 until sourceInstanceUris.size;
//            t <- 0 until targetInstanceUris.size)
//        {
//
//        }
//      }
//      catch
//      {
//        case ex : Exception => logger.log(Level.WARNING, "Could not execute match task", ex)
//      }
//
//      links
//    }
//
//
//
//    def compareIndexes(index1 : Set[BigInt], index2 : Set[BigInt]) =
//    {
//      index1.exists(index2.contains(_))
//    }
//
//    def evaluateCondition(instances : SourceTargetPair[Instance]) =
//    {
//      linkSpec.condition(instances, linkSpec.filter.threshold)
//    }
//}

