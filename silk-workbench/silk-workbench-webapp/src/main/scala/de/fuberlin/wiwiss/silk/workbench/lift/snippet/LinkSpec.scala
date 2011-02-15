package de.fuberlin.wiwiss.silk.workbench.lift.snippet

import net.liftweb.http.js.JE.{JsRaw}
import net.liftweb.http.js.JsCmds.Script
import net.liftweb.util.Helpers._
import net.liftweb.http.js.JE.{Call, Str, Num, JsArray}
import xml.NodeSeq
import net.liftweb.http.{S, SHtml}
import net.liftweb.http.js.{JsObj, JsCmds}
import de.fuberlin.wiwiss.silk.instance.Path
import java.io.StringReader
import de.fuberlin.wiwiss.silk.linkspec.LinkSpecification
import de.fuberlin.wiwiss.silk.workbench.workspace.User

class LinkSpec
{
  def toolbar(xhtml : NodeSeq) : NodeSeq =
  {
    bind("entry", xhtml,
         "update" -> SHtml.ajaxButton("Update", () => SHtml.ajaxCall(Call("serializeLinkSpec"), updateLinkSpec)._2.cmd),
         "download" -> SHtml.submit("Download", () => S.redirectTo("config")))
  }

  def content(xhtml : NodeSeq) : NodeSeq =
  {
    bind("entry", xhtml,
         "linkSpecVar" -> Script(generateLinkSpecVar))
  }

  /**
   * Updates the Link Specification
   */
  private def updateLinkSpec(linkSpecStr : String) =
  {
    try
    {
      val linkingTask = User().linkingTask

      //Load link specification
      val linkSpec = LinkSpecification.load(linkingTask.prefixes)(new StringReader(linkSpecStr))

      //Update linking task
      User().linkingTask = linkingTask.copy(linkSpec = linkSpec)

      JsRaw("alert('Updated Link Specification')").cmd
    }
    catch
    {
      case ex : Exception => JsRaw("alert('Error updating Link Specification. Details: " + ex.getMessage.encJs + "')").cmd
    }
  }

  private def generateLinkSpecVar() =
  {
    //Serialize the link condition to a JavaScript string
    val linkSpecStr = User().linkingTask.linkSpec.toXML.toString.replace("\n", " ")

    val linkSpecVar = "var linkSpec = '" + linkSpecStr + "';"

    JsRaw(linkSpecVar).cmd
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
