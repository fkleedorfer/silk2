package de.fuberlin.wiwiss.silk.workbench.lift.snippet

import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.JsRaw
import net.liftweb.json.JsonDSL._
import net.liftweb.json.JsonAST
import net.liftweb.http.js.JsCmds.{Script, OnLoad}
import xml.NodeSeq
import net.liftweb.http.js.{JsCmd, JsCmds}
import de.fuberlin.wiwiss.silk.workbench.workspace.User
import net.liftweb.json.JsonAST.{JObject, JArray, JValue}
import de.fuberlin.wiwiss.silk.datasource.DataSource

/**
 * Workspace snippet.
 *
 * Injects the following functions:
 *
 * def createProject()
 *
 * def removeProject(
 *     projectName : The name of the project
 * )
 *
 * def createSourceTask(
 *     projectName : The name of the project
 * )
 *
 * def editSourceTask(
 *     projectName : The name of the project
 *     taskName : The name of the task to be edited
 * )
 *
 * def removeSourceTask(
 *     projectName : The name of the project
 *     taskName : The name of the task to be removed
 * )
 *
 * def createLinkingTask(
 *     projectName : The name of the project
 * )
 *
 * def openLinkingTask(
 *     projectName : The name of the project
 *     taskName : The name of the task to be removed
 * )
 *
 * def removeLinkingTask(
 *     projectName : The name of the project
 *     taskName : The name of the task to be removed
 * )
 *
 * Whenever the workspace changes, the following function will be called:
 *
 * def updateWorkspace(
 *     workspace : JSON containing the workspace contents
 * )
 *
 */
class Workspace
{
  def content(xhtml : NodeSeq) : NodeSeq =
  {
    bind("entry", xhtml,
         "injectedJavascript" -> (Script(Workspace.javasScriptFunctions)))
  }
}

object Workspace
{
  def javasScriptFunctions =
  {
    updateWorkspaceCmd &
    createProjectFunction &
    removeProjectFunction &
    createSourceTaskFunction &
    editSourceTaskFunction &
    injectFunction("removeSourceTask", removeSourceTask _) &
    createLinkingTaskFunction &
    injectFunction("openLinkingTask", openLinkingTask _, true) &
    injectFunction("removeLinkingTask", removeLinkingTask _)
  }

  /**
   * JS Command which updates the workspace view.
   */
  def updateWorkspaceCmd : JsCmd =
  {
    JsRaw("var workspaceVar = " + pretty(JsonAST.render(workspaceJson)) + "; updateWorkspace(workspaceVar);").cmd
  }

  /**
   * JS Command which defines the createProject function
   */
  private def createProjectFunction : JsCmd =
  {
    val ajaxCall = SHtml.ajaxInvoke(CreateProjectDialog.openCmd _)._2.cmd

    CreateProjectDialog.initCmd & JsCmds.Function("createProject", Nil, ajaxCall)
  }

  /**
   * JS Command which defines the removeProject function
   */
  private def removeProjectFunction : JsCmd =
  {
    def callback(projectName : String) : JsCmd =
    {
      User().workspace.removeProject(projectName)

      updateWorkspaceCmd
    }

    val ajaxCall = SHtml.ajaxCall(JsRaw("projectName"), callback _)._2.cmd

    JsCmds.Function("removeProject", "projectName" :: Nil, ajaxCall)
  }

  /**
   * JS Command which defines the createDataSourceTask function
   */
  private def createSourceTaskFunction : JsCmd =
  {
    def callback(projectName : String) : JsCmd =
    {
      User().project = User().workspace.project(projectName)

      JsRaw("$('#createSourceTaskDialog').dialog('open');").cmd
    }

    val ajaxCall = SHtml.ajaxCall(JsRaw("projectName"), callback _)._2.cmd

    val initSourceTaskDialog = OnLoad(JsRaw("$('#createSourceTaskDialog').dialog({ autoOpen: false, width: 700, modal: true })").cmd)
    val openSourceTaskDialog =  JsCmds.Function("createSourceTask", "projectName" :: Nil, ajaxCall)

    initSourceTaskDialog & openSourceTaskDialog
  }

  /**
   * JS Command which defines the editSourceTask function
   */
  private def editSourceTaskFunction : JsCmd =
  {
    def callback(args : String) : JsCmd =
    {
      val Array(projectName, taskName) = args.split(',')

      User().project = User().workspace.project(projectName)
      User().task = User().project.sourceModule.task(taskName)

      EditSourceTaskDialog.openCmd
    }

    val ajaxCall = SHtml.ajaxCall(JsRaw("projectName + ',' + taskName"), callback _)._2.cmd

    EditSourceTaskDialog.initCmd & JsCmds.Function("editSourceTask", "projectName" :: "taskName" :: Nil, ajaxCall)
  }

  /**
   * Removes a source task from the workspace.
   */
  private def removeSourceTask(projectName : String, taskName : String)
  {
    User().workspace.project(projectName).sourceModule.remove(taskName)
  }

  /**
   * JS Command which defines the createLinkingTask function
   */
  private def createLinkingTaskFunction : JsCmd =
  {
    def callback(projectName : String) : JsCmd =
    {
      User().project = User().workspace.project(projectName)

      JsRaw("$('#createLinkingTaskDialog').dialog('open');").cmd
    }

    val ajaxCall = SHtml.ajaxCall(JsRaw("projectName"), callback _)._2.cmd

    val initLinkingTaskDialog = OnLoad(JsRaw("$('#createLinkingTaskDialog').dialog({ autoOpen: false, width: 700, modal: true })").cmd)
    val openLinkingTaskDialog =  JsCmds.Function("createLinkingTask", "projectName" :: Nil, ajaxCall)

    initLinkingTaskDialog & openLinkingTaskDialog
  }

  /**
   * Opens a linking task from the workspace.
   */
  private def openLinkingTask(projectName : String, taskName : String)
  {
    User().project = User().workspace.project(projectName)
    User().task = User().project.linkingModule.task(taskName)
  }

  /**
   * Removes a linking task from the workspace.
   */
  private def removeLinkingTask(projectName : String, taskName : String)
  {
    User().workspace.project(projectName).linkingModule.remove(taskName)
  }

  /*
   * Injects a Javascript function.
   */
  private def injectFunction(name : String, func : (String, String) => Unit, reload : Boolean = false) : JsCmd =
  {
    //Callback which executes the provided function
    def callback(args : String) : JsCmd =
    {
      try
      {
        val Array(projectName, taskName) = args.split(',')

        func(projectName, taskName)

        if(reload)
          updateWorkspaceCmd & JsRaw("window.location.reload();").cmd
        else
          updateWorkspaceCmd
      }
      catch
      {
        case ex : Exception => JsRaw("alert('" + ex.getMessage.encJs + "');").cmd
      }
    }

    //Ajax Call which executes the callback
    val ajaxCall = SHtml.ajaxCall(JsRaw("projectName + ',' + taskName"), callback _)._2.cmd

    //JavaScript function definition
    JsCmds.Function(name, "projectName" :: "taskName" :: Nil, ajaxCall)
  }

  /**
   * Generates a JSON which contains the workspace contents.
   */
  private def workspaceJson : JValue =
  {

    // TODO - Nested 'yield's seem to cause the (strange) compiler error: 'xxx is not an enclosing class'
    var projectList : List[JValue] = List()

    for(project <- User().workspace.projects.toSeq)
    {
      val sources : JArray = for(task <- project.sourceModule.tasks.toSeq) yield
      {
        task.source.dataSource match
        {
          case DataSource(_, params) =>
          {
            ("name" -> task.name.toString) ~
            ("params" -> paramsToJson(params))
          }
        }
      }

      val linkingTasks : JArray = for(task <- project.linkingModule.tasks.toSeq) yield
      {
        ("name" -> task.name.toString) ~
        ("source" -> task.linkSpec.datasets.source.sourceId.toString) ~
        ("target" -> task.linkSpec.datasets.target.sourceId.toString) ~
        ("sourceDataset" -> task.linkSpec.datasets.source.restriction) ~
        ("targetDataset" -> task.linkSpec.datasets.target.restriction)
      }

      val proj : JObject =
      {
        ("name" -> project.name.toString) ~
        ("dataSource" -> sources) ~
        ("linkingTask" -> linkingTasks)
      }

      projectList ::= proj
    }

    val projects = ("project" -> JArray(projectList))
    val activeProject = ("activeProject" -> (if(User().projectOpen) User().project.name.toString else ""))
    val activeTask = ("activeTask" -> (if(User().taskOpen) User().task.name.toString else ""))
    val activeTaskType = ("activeTaskType" -> (if(User().taskOpen) User().task.getClass.getSimpleName else ""))

    ("workspace" -> projects ~ activeProject ~ activeTask ~ activeTaskType)
  }

  private def paramsToJson(params : Map[String, String]) : JArray =
  {
    for((key, value) <- params.toSeq) yield
    {
      ("key" -> key) ~
      ("value" -> value)
    }
  }
}
