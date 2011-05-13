package de.fuberlin.wiwiss.silk.impl.comparisonvectorhandler

import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import java.util.logging.Logger
import java.io._
import de.fuberlin.wiwiss.silk.linkspec.condition.FeatureInstance
import util.Random
import de.fuberlin.wiwiss.silk.instance.Instance
import de.fuberlin.wiwiss.silk.util.SourceTargetPair
import de.fuberlin.wiwiss.silk.output.OutputRow
import collection.mutable.HashSet
import de.fuberlin.wiwiss.silk.linkspec.FeatureVectorOutput


@StrategyAnnotation(id = "csv", label = "Csv", description = "output each comparison vector in csv format")
class CsvFeatureVectorOutput(file: String, fieldSeparator: String = ";") extends FeatureVectorOutput {
    private val logger = Logger.getLogger(classOf[CsvFeatureVectorOutput].getName)
    private val outfile = new File(file)
    private var writer:PrintWriter = null
    private val random = new Random(System.currentTimeMillis())
    private var header:Seq[String] = Seq.empty

    //TODO: this never gets called
    def closeWriter() = {
        writer.close();
    }

    override def initialize(header:Seq[String]) =
    {
      this.header = header
      this.writer = new PrintWriter(new FileOutputStream(outfile))
    }

    override def apply(fields: Seq[String]) = {
        if (!CsvFeatureVectorOutput.initializedOutFiles(outfile)){
          CsvFeatureVectorOutput.initialize(outfile,header.mkString(fieldSeparator),writer)
        }
        writer.println(fields.map(_.replaceAll("(\n|\r\n)"," ")).mkString(fieldSeparator))
        writer.flush()
    }

}

object CsvFeatureVectorOutput {

  private var initializedOutFiles:scala.collection.mutable.Set[File] = new HashSet[File]
  var lock : AnyRef = new Object()

  private def initialize(outFile:File, header:String, writer: PrintWriter) = {
    lock.synchronized {
      if (!initializedOutFiles(outFile)) {
        writer.println(header)
        writer.flush()
        initializedOutFiles.add(outFile)
      }
    }
  }
}