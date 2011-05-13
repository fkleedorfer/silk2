package de.fuberlin.wiwiss.silk.linkspec

/**
 * <DESCRIPTION>
 * 
 * <p>
 * <b>Company:&nbsp;</b> SAT, Research Studios Austria
 * </p>
 * 
 * <p>
 * <b>Copyright:&nbsp;</b> (c) 2011
 * </p>
 * 
 * <p>
 * <b>last modified:</b><br/>
 * $Author: $<br/>
 * $Date: $<br/>
 * $Revision: $
 * </p>
 * 
 * @author Florian Kleedorfer
 */
object BlockingMode extends Enumeration {
       val Strict, Lax, Disabled = Value
  def fromString(strVal: String): Value =
  {
    strVal match {
      case "strict" => Strict
      case "lax" => Lax
      case "disabled" => Disabled
      case _ => throw new IllegalArgumentException("BlockingMode must be one of 'strict', 'lax' or 'disabled'")
    }
  }
}