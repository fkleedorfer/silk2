package de.fuberlin.wiwiss.silk.workbench.workspace.io

import de.fuberlin.wiwiss.silk.datasource.Source
import xml.{Node, NodeSeq}
import de.fuberlin.wiwiss.silk.config.Prefixes
import de.fuberlin.wiwiss.silk.linkspec.LinkSpecification
import de.fuberlin.wiwiss.silk.evaluation.AlignmentReader
import de.fuberlin.wiwiss.silk.workbench.workspace.{ProjectConfig, Project}
import de.fuberlin.wiwiss.silk.workbench.workspace.modules.source.SourceTask
import de.fuberlin.wiwiss.silk.workbench.workspace.modules.linking.{LinkingTask, Cache}

/**
 * Reads a project from a single XML file.
 */
object ProjectImporter
{
  def apply(project : Project, xml : NodeSeq) =
  {
    implicit val prefixes = Prefixes.fromXML(xml \ "Prefixes" head)

    project.config = ProjectConfig(prefixes)

    for(taskNode <- xml \ "SourceModule" \ "Tasks" \ "SourceTask")
    {
      project.sourceModule.update(readSourceTask(taskNode))
    }

    for(taskNode <- xml \ "LinkingModule" \ "Tasks" \ "LinkingTask")
    {
      project.linkingModule.update(readLinkingTask(taskNode))
    }
  }

  private def readSourceTask(xml : Node) =
  {
    SourceTask(Source.fromXML(xml \ "_" head))
  }

  private def readLinkingTask(xml : Node)(implicit prefixes : Prefixes) =
  {
    val linkSpec = LinkSpecification.fromXML(xml \ "LinkSpecification" \ "_" head)
    val alignment = AlignmentReader.readAlignment(xml \ "Alignment" \ "_" head)
    val cache = Cache.fromXML(xml \ "Cache" \ "_" head)

    LinkingTask(linkSpec, alignment, cache)
  }
}