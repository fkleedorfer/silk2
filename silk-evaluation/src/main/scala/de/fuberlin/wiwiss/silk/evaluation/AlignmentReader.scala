package de.fuberlin.wiwiss.silk.evaluation

import io.Source
import de.fuberlin.wiwiss.silk.output.Link
import java.io.{InputStream, File}
import xml.{Node, XML}

/**
 * Reads the alignment format specified at http://alignapi.gforge.inria.fr/format.html.
 */
object AlignmentReader
{
  def read(file : File) : Alignment =
  {
    file.getName.split("\\.").last match
    {
      case "n3debug" => readN3Debug(file)
      case "xml" => readAlignment(file)
      case format => throw new IllegalArgumentException("Unsupported format: " + format)
    }
  }

  def readAlignment(inputStream : InputStream) : Alignment =
  {
    readAlignment(XML.load(inputStream))
  }

  def readAlignment(file : File) : Alignment =
  {
    readAlignment(XML.loadFile(file))
  }

  private def readAlignment(xml : Node) : Alignment =
  {
    new Alignment(readLinks(xml, "=").toSet, readLinks(xml, "!=").toSet)
  }

  private def readLinks(xml : Node, relation : String) : Traversable[Link] =
  {
    for(cell <- xml \ "Alignment" \ "map" \ "Cell" if (cell \ "relation" text) == relation) yield
    {
      new Link(sourceUri = cell \ "entity1" \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}resource" text,
               targetUri = cell \ "entity2" \ "@{http://www.w3.org/1999/02/22-rdf-syntax-ns#}resource" text,
               confidence = (cell \ "measure").text.toDouble)
    }
  }

  private val N3DebugRegex = """\[(\d*\.\d*)\]:\s*<([^>]*)>\s*<[^>]*>\s*<([^>]*)>\s*\.\s*""".r

  def readN3Debug(file : File) : Alignment =
  {
    val source = Source.fromFile(file)
    val positiveLinks =
      for(N3DebugRegex(confidence, sourceUri, targetUri) <- source.getLines()) yield
      {
        new Link(sourceUri, targetUri, confidence.toDouble)
      }

    new Alignment(positiveLinks.toSet, Set.empty)
  }
}
