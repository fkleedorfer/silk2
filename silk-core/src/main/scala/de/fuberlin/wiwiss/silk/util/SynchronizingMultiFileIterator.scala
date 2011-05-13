package de.fuberlin.wiwiss.silk.util

import java.io.{Reader, BufferedReader, FileReader, File}
import concurrent.SyncChannel
import java.util.logging.Logger
import collection.mutable.MultiMap
import collection.immutable.{HashMap, Set}

/**
 * <DESCRIPTION>
 * 
 * <p>
 * <b>Company:&nbsp;</b> SAT, Research Studios Austria
 * </p>
 * 
 * <p>
 * <b>Copyright:&nbsp;</b> (c) 2011
 * </p>
 * 
 * <p>
 * <b>last modified:</b><br/>
 * $Author: $<br/>
 * $Date: $<br/>
 * $Revision: $
 * </p>
 * 
 * @author Florian Kleedorfer
 */
class SynchronizingMultiFileIterator[T,R <: Reader] (deserialize: R => Option[T], createReader: File => R, ordering: Ordering[T])
{
  var iteratingFileDeserializers:Seq[IteratingDeserializer] = Seq.empty
  var nextItems = new collection.mutable.HashMap[T, collection.mutable.Set[IteratingDeserializer]]() with MultiMap[T, IteratingDeserializer]
  val channel = new SyncChannel[Option[T]]()
  val innerThread = new InnerThread()
  val monitor = new AnyRef()
  val writeMonitor = new AnyRef()
  val MAX_WAIT_TIMEOUT: Long = 1000*5
  var skippedDuplicatesCount:Int = 0
  var counter:Int = 0
  var isWriting = false
  var stopWhenAllProcessed = false

  var nextItem: Option[T] = None

  private val logger = Logger.getLogger(getClass().getName())

  class InnerThread extends Thread
  {
    var running = true
    def abort(){
      monitor.synchronized
      {
        logger.info("aborting inner thread of SynchronizingMultiFileIterator")
        running = false
        monitor.notifyAll
        interrupt
      }
    }

    override def run()
    {
      logger.info("starting inner thread of SynchronizingMultiFileIterator")
      while(running)
      {
          //logger.info("checking all " + iteratingFileDeserializers.size + " iteratingFileDeserializers ...")
          //check if there is a next object
          prepareNext
          if (hasMoreData) {
            //logger.info("writing object to channel")
            //if yes, write it to the channel
            isWriting = true
            channel.write(Some(next()))
            isWriting = false
            //logger.info("done writing object to channel")
          } else {
            if (stopWhenAllProcessed){
              channel.write(None)
              running = false
            } else {
              //logger.info("no object to write, waiting..")
              //if not, wait on the monitor
              monitor.synchronized
              {
                monitor.wait(MAX_WAIT_TIMEOUT)
              }
              //logger.info("waited long enough, retrying")
            }
          }
      }
      logger.info("shutting down inner thread of SynchronizingMultiFileIterator")
    }
  }

  def start()
  {
    innerThread.start()
  }

  def stop()
  {
    innerThread.abort()
  }

  def setStopWhenAllProcessed(doStopWhenAllProcessed: Boolean) =
  {
    this.stopWhenAllProcessed = doStopWhenAllProcessed
  }

  def addFile(file: File)
  {
    logger.info("adding file " + file)
    monitor.synchronized
    {
      val newDeserializer = new IteratingFileDeserializer(file, deserialize, createReader)
      //go to first item
      if (newDeserializer.hasNext) {
        iteratingFileDeserializers = iteratingFileDeserializers :+ newDeserializer
        nextItems.addBinding(newDeserializer.next, newDeserializer)
        this.monitor.notify
        logger.info("added deserializer for file: " + file)
      } else {
        logger.info("file seems to be empty - ignoring: " + file)
      }
    }
  }

  /**
   * Returns the next item read from one of the files. Blocks until there is an item available.
   * If the last item has already been read and stopIfAllProcessed is true, this method returns None
   * to signal that processing should end.
   */
  def read(): Option[T] =
  {
    //logger.info("reading...")
    val result = channel.read
    //logger info("read item " + result)
    result
  }

  def skippedDuplicates(): Int =
  {
    skippedDuplicatesCount
  }

  def hasMoreData(): Boolean =
  {
    monitor.synchronized
    {
      isWriting || this.nextItem.isDefined
    }
  }

  private def prepareNext() =
  {
    var loadCount = 0

    monitor.synchronized
    {
       //hack: nextItem must be None here!
      if (nextItem.isEmpty && nextItems.size > 0 && iteratingFileDeserializers.size > 0){
        //the nextItems list has been filled with the latest item from each file
        //the smallest item will be taken as the 'nextItem'
        //its place in the list is replaced by None. (If the nextItems list contains this value
        //multiple times, all will be replaced by None)
        //then, all None values are replaced by the next() item from the corresponding file. If that item
        //is not available, it means that the corresponding file has been read completely. The position is erased
        //in both lists

        //get the smallest item:
        nextItem = Some(nextItems.keys.min(ordering))

        val firstDeserializers = nextItems.get(nextItem.get).get
        for (deserializer:IteratingDeserializer <- firstDeserializers)
        {
          //remove the binding
          nextItems.removeBinding(nextItem.get, deserializer)
          if (deserializer.hasNext)
            {
              //fetch the next item
              var nextItemForPosition = deserializer.next
              loadCount += 1
              //compare with latest 'nextItem', if they are equal, fetch another one - until EOF or different item
              while(nextItemForPosition.equals(nextItem.get) && deserializer.hasNext)
              {
                nextItemForPosition = deserializer.next
                loadCount += 1
              }
              if (deserializer.hasNext)
              {
                //we found a different item: add a binding to the nextItems multi map
                nextItems.addBinding(nextItemForPosition, deserializer)
              }
            }
        }
        this.skippedDuplicatesCount += loadCount - 1
      }
    }
  }

  def printStats() =
  {
     var info = " managing " + iteratingFileDeserializers.size + " files\n"
     info += " produced " + counter + " items so far \n skipped  " + skippedDuplicates + " duplicates \n hasMoreData=" + hasMoreData
     for (d <- iteratingFileDeserializers)
     {
       info += ("\n " +d.toString)
     }
    logger.info(info)
  }

  private def next(): T =
  {
    monitor.synchronized
    {
      if (nextItem.isDefined) {
        counter += 1
        val ret = nextItem.get
        nextItem = None
        prepareNext()
        ret
      } else {
        throw new NoSuchElementException("next item is not available")
      }
    }
  }



  trait IteratingDeserializer
  {
    def hasNext(): Boolean
    def next(): T
  }


  class IteratingFileDeserializer (file: File, deserialize: R => Option[T], createReader: File => R) extends IteratingDeserializer
  {
    val reader: R = createReader(file)
    var atStart = true
    var nextItem: Option[T] = None
    var currentItem: Option[T] = None
    var lineCounter:Int = 0

    override def next(): T =
    {
      if (atStart){
        atStart = false
        readNext
      }
      if (nextItem.isDefined) {
        val ret = nextItem.get
        readNext()
        ret
      } else {
        throw new NoSuchElementException("next item is not available")
      }
    }



    private def readNext() =
    {
      //logger.info("reading next item from file " + file)
      try
      {
        nextItem = deserialize(reader)
        //logger.info("reading next item yields " + currentObject)
        if (nextItem.isEmpty) {
          logger.info("reached end of file " + file + ", deleting it")
          reader.close()
          file.delete
        } else {
          lineCounter += 1
        }
      }
      catch
      {
        case ex: java.io.EOFException => {
          logger.info("reached end of file " + file + ", deleting it")
          nextItem = None
          reader.close()
          file.delete
        }
        case ex: Exception =>{
          logger.info("caught exception when deserializing from file " + file + ": "+ ex.getMessage)
        }
      }
    }

    override def hasNext(): Boolean =
    {
      if (atStart){
        readNext
        atStart = false
      }
      nextItem.isDefined
    }

    override def toString() =
    {
      "IteratingFileDeserializer: " + file + ": at line " + lineCounter
    }
  }
}