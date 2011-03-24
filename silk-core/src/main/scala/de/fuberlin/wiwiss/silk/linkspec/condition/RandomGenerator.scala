package de.fuberlin.wiwiss.silk.linkspec.condition

import de.fuberlin.wiwiss.silk.util.strategy.{Strategy, Factory}

trait RandomGenerator extends Strategy
{
  def nextValue() : Double

  def index() : Set[Seq[Int]]

  val blockCounts: Seq[Int]
}

object RandomGenerator extends Factory[RandomGenerator]