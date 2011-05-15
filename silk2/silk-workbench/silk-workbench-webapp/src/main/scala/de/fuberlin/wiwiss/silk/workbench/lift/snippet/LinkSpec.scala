package de.fuberlin.wiwiss.silk.workbench.lift.snippet

import net.liftweb.http.js.JE.JsRaw
import net.liftweb.http.js.JsCmds.Script
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE.Call
import xml.NodeSeq
import net.liftweb.http.{S, SHtml}
import java.io.StringReader
import de.fuberlin.wiwiss.silk.linkspec.LinkSpecification
import de.fuberlin.wiwiss.silk.workbench.workspace.User
import net.liftweb.http.js.{JsCmd, JsCmds}
import net.liftweb.http.js.JsCmds.OnLoad
import de.fuberlin.wiwiss.silk.workbench.lift.util.JavaScriptUtils.Redirect
import de.fuberlin.wiwiss.silk.MatchTask
import java.util.logging.{Level, Logger}

/**
 * LinkSpec snippet.
 *
 * Injects the 'linkSpec' variable which holds the current link spec.
 * Defines the 'reloadCache()' function which reloads the current cache containing the property paths.
 * Calls the serializeLinkSpec() function from the editor whenever the user saves the current link specification.
 */
class LinkSpec
{
  private val logger = Logger.getLogger(classOf[LinkSpec].getName)

  /**
   * Renders the toolbar.
   */
  def toolbar(xhtml : NodeSeq) : NodeSeq =
  {
    //JS Command which saves the current link specification
    def saveCall(close : Boolean) = SHtml.ajaxCall(Call("serializeLinkSpec"), saveLinkSpec(close))._2.cmd

    //JS Command which closes the current link specification
    def closeCall = SHtml.ajaxInvoke(closeLinkSpec)._2.cmd

    //Initializes the close dialog
    def initDialog = Script(OnLoad(JsRaw("""
      $('#dialog-confirm').dialog({
        autoOpen: false,
        resizable: false,
        height: 140,
        modal: true,
        buttons: {
          Yes: function() { """  + saveCall(true).toJsCmd + """ $(this).dialog('close'); },
          No: function() { """ + closeCall.toJsCmd + """$(this).dialog('close'); }
        }
      });""").cmd))

    //JS Command which opens the close dialog
    def openDialog = JsRaw("$('#dialog-confirm').dialog('open');").cmd

    bind("entry", xhtml,
         "close" -> (initDialog ++ SHtml.ajaxButton("Close", openDialog _)),
         "save" -> SHtml.ajaxButton("Save", () => saveCall(false)),
         "export" -> SHtml.ajaxButton("Export as Silk-LS", () => Redirect("config.xml")))
  }

  /**
   * Renders the content.
   */
  def content(xhtml : NodeSeq) : NodeSeq =
  {
    bind("entry", xhtml,
         "linkSpecVar" -> Script(generateLinkSpecVar & reloadCacheFunction))
  }

  /**
   * Saves the Link Specification.
   */
  private def saveLinkSpec(closeOnSuccess : Boolean)(linkSpecStr : String) =
  {
    try
    {
      val project = User().project
      val linkingTask = User().linkingTask
      implicit val prefixes = project.config.prefixes

      //Load link specification
      val linkSpec = LinkSpecification.load(prefixes)(new StringReader(linkSpecStr))

      //Update linking task
      val updatedLinkingTask = linkingTask.copy(linkSpec = linkSpec)

      //Commit
      project.linkingModule.update(updatedLinkingTask)
      User().task = updatedLinkingTask

      if(closeOnSuccess)
      {
        SHtml.ajaxInvoke(closeLinkSpec)._2.cmd
      }
      else
      {
        JsRaw("alert('Saved')").cmd
      }
    }
    catch
    {
      case ex : Exception =>
      {
        logger.log(Level.INFO, "Failed to save link specification", ex)
        JsRaw("alert('Error updating Link Specification.\\n\\nDetails: " + ex.getMessage.encJs + ".');").cmd
      }
    }
  }

  /**
   * Closes the current link specification.
   */
  private def closeLinkSpec() =
  {
    try
    {
      User().closeTask()

      Redirect("index.html")
    }
    catch
    {
      case ex : Exception => JsRaw("alert('Error closing Editor. Details: " + ex.getMessage.encJs + "')").cmd
    }
  }

  /**
   * Generates the 'linkSpec' variable which holds the current link specification.
   */
  private def generateLinkSpecVar() =
  {
    val linkingTask = User().linkingTask
    implicit val prefixes = User().project.config.prefixes

    //Serialize the link condition to a JavaScript string
    val linkSpecStr = linkingTask.linkSpec.toXML.toString.replace("\n", " ").replace("\\", "\\\\")

    val linkSpecVar = "var linkSpec = '" + linkSpecStr + "';"

    JsRaw(linkSpecVar).cmd
  }

  /**
   * JS Command which defines the reloadCache function
   */
  private def reloadCacheFunction : JsCmd =
  {
    def reloadCache =
    {
      User().linkingTask.cache.reload(User().project, User().linkingTask)
      JsRaw("").cmd
    }

    JsCmds.Function("reloadCache", Nil, SHtml.ajaxInvoke(reloadCache _)._2.cmd)
  }

//  private def generatePathsFunction() =
//  {
//    JsCmds.Function("retrievePaths", Nil, JsCmds.JsReturn(SHtml.ajaxInvoke(() => Str("test").cmd)._2))
//  }
//
//  private def generatePathsObj() =
//  {
//    new JsObj
//    {
//      val props = ("source", generateSelectedPathsObj(true)) ::
//                  ("target", generateSelectedPathsObj(false)) :: Nil
//    }.cmd
//  }
//
//  private def generateSelectedPathsObj(selectSource : Boolean) =
//  {
//    val dataset = Project().linkSpec.datasets.select(selectSource)
//
//    val instanceSpec = Project().cache.instanceSpecs.select(selectSource)
//
//    new JsObj
//    {
//      val props = ("id", Str(dataset.sourceId)) ::
//                  ("paths", JsArray(instanceSpec.paths.map(generatePathObj) : _*)) ::
//                  ("availablePaths", Num(instanceSpec.paths.size)) ::
//                  ("restrictions", Str(instanceSpec.restrictions)) :: Nil
//    }
//  }
//
//  private def generatePathObj(path : Path) =
//  {
//    new JsObj
//    {
//      val props = ("path", Str(path.toString)) ::
//                  ("frequency", Num(1.0)) :: Nil
//    }
//  }
}
