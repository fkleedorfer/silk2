package de.fuberlin.wiwiss.silk.util

/**
 * Thrown if the configuration is not valid.
 */
class ValidationException(e : String, cause : Throwable) extends Exception(e)
{
  def this(e : String) = this(e, null)
}