package de.fuberlin.wiwiss.silk.impl.classifier

import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation
import util.Random
import de.fuberlin.wiwiss.silk.linkspec.condition.RandomGenerator

@StrategyAnnotation(
  id = "boolean",
  label = "Boolean Random Generator",
  description = "Generates random boolean values {0.0,1.0}. true (1.0) is generated with the specified probability. ")
class BooleanRandomGenerator(trueProbability:Double, blocks: Int) extends RandomGenerator
{
  require(trueProbability >= 0.0 && trueProbability <= 1.0, "TrueProbability must be in [0.0, 1.0]")

  private val random = new Random(System.currentTimeMillis())

  override def nextValue() : Double =
  {
    val rnd = random.nextDouble()
    //System.out.println("generating 1.0 with probability " + trueProbability + " random value: " + rnd)
    if (rnd < trueProbability){
      1.0
    } else {
      0.0
    }
  }

  def index(instanceUri:String) : Set[Seq[Int]] = {
    return Set(Seq(instanceUri.hashCode % blockCounts(0)))
    //Set(Seq(random.nextInt(blocks)))
    //Set(Seq(1))
  }

  override val blockCounts : Seq[Int] =
  {
    if (blocks < 1){
      Seq(1)
    } else {
      Seq(blocks)
    }
  }

}