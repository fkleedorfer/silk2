package de.fuberlin.wiwiss.silk.linkspec.transformer

import org.scalatest.matchers.{ShouldMatchers}
import org.scalatest.FlatSpec
import de.fuberlin.wiwiss.silk.impl.transformer.ReplaceTransformer
import de.fuberlin.wiwiss.silk.impl.DefaultImplementations

class ReplaceTransformerTest extends FlatSpec with ShouldMatchers
{
    DefaultImplementations.register()

    val transformer = new ReplaceTransformer(search = " ", replace = "")

    "ReplaceTransformer" should "return 'abc'" in
    {
        transformer.evaluate(List("a b c")) should equal ("abc")
    }

    val transformer1 = new ReplaceTransformer(search = "abc", replace = "")

    "ReplaceTransformer" should "return 'def'" in
    {
        transformer1.evaluate(List("abcdef")) should equal ("def")
    }
}