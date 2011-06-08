package de.fuberlin.wiwiss.silk.workbench.lift.comet

import collection.mutable.{Publisher, Subscriber}
import de.fuberlin.wiwiss.silk.util.Task
import de.fuberlin.wiwiss.silk.workbench.workspace.User
import de.fuberlin.wiwiss.silk.output.Link
import net.liftweb.http.SHtml
import xml.NodeSeq
import de.fuberlin.wiwiss.silk.workbench.workspace.UserData._
import net.liftweb.http.js.JsCmds.{OnLoad, SetHtml, Script, JsShowId, JsHideId}
import de.fuberlin.wiwiss.silk.workbench.evaluation.EvalLink
import de.fuberlin.wiwiss.silk.workbench.evaluation.EvalLink.{Correct, Incorrect, Undecided, Positive, Negative, Generated}

class GeneratedLinks extends LinkList
{
  /** Minimum time in milliseconds between two successive updates*/
  private val minUpdatePeriod = 3000L

  /** The time of the last update */
  private var lastUpdateTime = 0L

  override protected val showStatus = false

  /** Register to status messages of the evaluation task in order to be notified when new links are available */
  User().evaluationTask.subscribe(new Subscriber[Task.StatusMessage, Publisher[Task.StatusMessage]]
  {
    def notify(pub : Publisher[Task.StatusMessage], status : Task.StatusMessage)
    {
      status match
      {
        case Task.Started() =>
        {
        }
        case Task.StatusChanged(_, _) if System.currentTimeMillis - lastUpdateTime > minUpdatePeriod =>
        {
          partialUpdate(updateLinksCmd)
          lastUpdateTime = System.currentTimeMillis
        }
        case Task.Finished(_, _) =>
        {
          partialUpdate(updateLinksCmd)
        }
        case _ =>
      }
    }
  })

  override protected def links : Seq[EvalLink] =
  {
    val linkingTask = User().linkingTask
    def alignment = linkingTask.alignment

    for(link <- User().evaluationTask.links.view) yield
    {
      if(alignment.positive.contains(link))
      {
        new EvalLink(link, Correct, Generated)
      }
      else if(alignment.negative.contains(link))
      {
        new EvalLink(link, Incorrect, Generated)
      }
      else
      {
        new EvalLink(link, Undecided, Generated)
      }
    }
  }

  override protected def renderStatus(link : EvalLink) : NodeSeq =
  {
    link.correct match
    {
      case Correct => <div>correct</div>
      case Incorrect => <div>wrong</div>
      case Undecided => <div>unknown</div>
    }
  }

  override protected def renderButtons(link : EvalLink) : NodeSeq =
  {
    <div id={getId(link, "confirmedLink")} style={if(link.correct == Correct) "display:block" else "display:none"}>
      <a><img src="./static/img/confirm.png" /></a>
      {SHtml.a(() => resetLink(link), <img src="./static/img/undecided-disabled.png" />)}
      {SHtml.a(() => declineLink(link), <img src="./static/img/decline-disabled.png" />)}
    </div>
    <div id={getId(link, "declinedLink")} style={if(link.correct == Incorrect) "display:block" else "display:none"}>
      {SHtml.a(() => confirmLink(link), <img src="./static/img/confirm-disabled.png" />)}
      {SHtml.a(() => resetLink(link), <img src="./static/img/undecided-disabled.png" />)}
       <a><img src="./static/img/decline.png" /></a>
    </div>
    <div id={getId(link, "undecidedLink")} style={if(link.correct == Undecided) "display:block" else "display:none"}>
      {SHtml.a(() => confirmLink(link), <img src="./static/img/confirm-disabled.png" />)}
       <a><img src="./static/img/undecided.png" /></a>
      {SHtml.a(() => declineLink(link), <img src="./static/img/decline-disabled.png" />)}
    </div>
  }

  private def confirmLink(link : Link) =
  {
    val linkingTask = User().linkingTask
    val alignment = linkingTask.alignment
    val updatedTask = linkingTask.copy(alignment = alignment.copy(positive = alignment.positive + link, negative = alignment.negative - link))

    User().project.linkingModule.update(updatedTask)
    User().task = updatedTask

    JsShowId(getId(link, "confirmedLink")) & JsHideId(getId(link, "declinedLink")) & JsHideId(getId(link, "undecidedLink"))
  }

  private def declineLink(link : Link) =
  {
    val linkingTask = User().linkingTask
    val alignment = linkingTask.alignment
    val updatedTask = linkingTask.copy(alignment = alignment.copy(positive = alignment.positive - link, negative = alignment.negative + link))

    User().project.linkingModule.update(updatedTask)
    User().task = updatedTask

    JsShowId(getId(link, "declinedLink")) & JsHideId(getId(link, "confirmedLink")) & JsHideId(getId(link, "undecidedLink"))
  }

  private def resetLink(link : Link) =
  {
    val linkingTask = User().linkingTask
    val alignment = linkingTask.alignment
    val updatedTask = linkingTask.copy(alignment = alignment.copy(positive = alignment.positive - link, negative = alignment.negative - link))

    User().project.linkingModule.update(updatedTask)
    User().task = updatedTask

    JsShowId(getId(link, "undecidedLink")) & JsHideId(getId(link, "confirmedLink")) & JsHideId(getId(link, "declinedLink"))
  }
}
