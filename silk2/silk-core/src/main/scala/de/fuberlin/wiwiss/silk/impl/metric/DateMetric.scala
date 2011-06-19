package de.fuberlin.wiwiss.silk.impl.metric

import de.fuberlin.wiwiss.silk.util.StringUtils._
import scala.math._
import javax.xml.datatype.{DatatypeConstants, XMLGregorianCalendar, DatatypeFactory}
import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import de.fuberlin.wiwiss.silk.linkspec.condition.SimpleDistanceMeasure

@StrategyAnnotation(
  id = "date",
  label = "Date",
  description = "The distance in days between two dates ('YYYY-MM-DD' format).")
class DateMetric(maxDays : Int) extends SimpleDistanceMeasure
{
  override def evaluate(str1 : String, str2 : String, threshold : Double) =
  {
    try
    {
      val datatypeFactory = DatatypeFactory.newInstance

      val date1 = datatypeFactory.newXMLGregorianCalendar(str1)
      val date2 = datatypeFactory.newXMLGregorianCalendar(str2)

      abs(totalDays(date1) - totalDays(date2)).toDouble
    }
    catch
    {
      case ex : IllegalArgumentException => Double.PositiveInfinity
    }
  }

  private def totalDays(date : XMLGregorianCalendar) =
  {
    val days = date.getDay match
    {
      case DatatypeConstants.FIELD_UNDEFINED => 0
      case d => d
    }

    val monthDays = date.getMonth match
    {
      case DatatypeConstants.FIELD_UNDEFINED => 0
      case m => m * 30
    }

    val yearDays = date.getYear match
    {
      case DatatypeConstants.FIELD_UNDEFINED => 0
      case y => y * 365
    }

    days + monthDays + yearDays
  }
}
