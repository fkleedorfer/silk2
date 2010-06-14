package de.fuberlin.wiwiss.silk.linkspec.metric

import org.scalatest.matchers.ShouldMatchers
import de.fuberlin.wiwiss.silk.linkspec.util.approximatelyEqualTo
import org.scalatest.FlatSpec

class DateMetricTest extends FlatSpec with ShouldMatchers
{
    val metric = new DateMetric(Map("max_days" -> "10"))

    "DateMetric" should "not return values under 0.0" in
    {
        metric.evaluate("2003-03-01", "2010-09-30") should be (approximatelyEqualTo (0.0))
    }

    "DateMetric" should "return 1.0 if the dates are equal" in
    {
        metric.evaluate("2010-09-30", "2010-09-30") should be (approximatelyEqualTo (1.0))
    }

    "DateMetric" should "ignore the time of day" in
    {
        metric.evaluate("2010-09-24", "2010-09-30") should be (approximatelyEqualTo (0.4))
        metric.evaluate("2010-09-24-06:00", "2010-09-30-06:00") should be (approximatelyEqualTo (0.4))
    }
}
