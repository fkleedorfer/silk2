package de.fuberlin.wiwiss.silk.workbench.workspace

import java.net.URI
import java.util.logging.Logger
import de.fuberlin.wiwiss.silk.config.Prefixes
import de.fuberlin.wiwiss.silk.util.sparql.RemoteSparqlEndpoint
import de.fuberlin.wiwiss.silk.workbench.util._

class LDEWorkspace (workspaceUri : URI) extends Workspace    {

  private val logger = Logger.getLogger(classOf[LDEProject].getName)

  private val prefixes = Prefixes (QueryFactory.getPrefixes)

  private val sparqlEndpoint = new RemoteSparqlEndpoint(new URI(workspaceUri+"/sparql"), prefixes)
  private val sparulEndpoint = new RemoteSparulEndpoint(new URI(workspaceUri+"/sparul"), prefixes)

  private var projectList : List[Project] = {

    val res = sparqlEndpoint.query(QueryFactory.sMappings,100)

    for(projectRes <- res.toList) yield  {
      val projectUri = projectRes("uri").value
      logger.info("Loading Project: "+projectUri)
      new LDEProject(clean(projectUri),sparqlEndpoint,sparulEndpoint)
    }
  }

  override def projects : List[Project] = projectList

  override def createProject(name : String) = {
    logger.info ("Creating new Project: "+name  )
    // TODO check if it already exists..
    sparulEndpoint.query(QueryFactory.iNewProject(name))
    val newProject = new LDEProject(name,sparqlEndpoint,sparulEndpoint)
    projectList ::= newProject
    newProject
  }

  override def removeProject(name : String) =  {
    logger.info ("Deleting Project: "+name  )
    sparulEndpoint.query(QueryFactory.dProject(name))
    projectList = projectList.filterNot(_.name == name)
  }

  def dataSourceList : List[String] = {
    val res = sparqlEndpoint.query(QueryFactory.sDataSources)
    logger.warning(QueryFactory.sDataSources)
    for(datasource <- res.toList) yield clean (datasource("uri").value )  
  }

  // util
  def clean (uri : String) =  {   uri.split("/").last  }


}

