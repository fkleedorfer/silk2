package de.fuberlin.wiwiss.silk.workbench.workspace

import java.io.File
import modules.linking.LinkingTask
import java.net.URI
import modules.ModuleTask
import modules.source.SourceTask
import de.fuberlin.wiwiss.silk.util.Task

/**
 * Dummy user as there is no user management yet.
 */
trait User
{
  private var currentProject : Option[Project] = None

  private var currentTask : Option[ModuleTask] = None

  /**
   * The current workspace of this user.
   */
  def workspace : Workspace

  def projectOpen = currentProject.isDefined

  /**
   * The current project of this user.
   */
  def project = currentProject.getOrElse(throw new NoSuchElementException("No active project"))

  /**
   * Sets the current project of this user.
   */
  def project_=(project : Project) =
  {
    currentProject = Some(project)
  }

  /**
   * True if a task if open at the moment.
   */
  def taskOpen = currentTask.isDefined

  /**
   * The current task of this user.
   */
  def task = currentTask.getOrElse(throw new NoSuchElementException("No active task"))

  /**
   * Sets the current task of this user.
   */
  def task_=(task : ModuleTask) =
  {
    currentTask = Some(task)
  }

  def closeTask()
  {
    currentTask = None
  }

  /**
   *   True, if a source task is open at the moment.
   */
  def sourceTaskOpen = taskOpen && task.isInstanceOf[SourceTask]

  /**
   * The current source task of this user.
   *
   * @throws java.util.NoSuchElementException If no source task is open
   */
  def sourceTask = task match
  {
    case t : SourceTask => t
    case _ => throw new NoSuchElementException("Active task is no source task")
  }

  /**
   * True, if a linking task is open at the moment.
   */
  def linkingTaskOpen = taskOpen && task.isInstanceOf[LinkingTask]

  /**
   * The current linking tasks of this user.
   *
   * @throws java.util.NoSuchElementException If no linking task is open
   */
  def linkingTask = task match
  {
    case t : LinkingTask => t
    case _ => throw new NoSuchElementException("Active task is no linking task")
  }
}

object User
{
  var userManager : () => User = () => throw new Exception("No user manager registerd")

  /**
   * Retrieves the current user.
   */
  def apply() =  userManager()
}