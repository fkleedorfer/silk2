package de.fuberlin.wiwiss.silk.hadoop.load

import java.io.IOException

//TODO add graph
class Quad(val subject : String, val predicate : String, val value : String)

object Quad
{
  def parse(line : String) : Quad =
  {
    line.split(' ') match
    {
      case Array(subject, predicate, value) => new Quad(subject, predicate, value)
      case _ => throw new IOException("Invalid line '" + line + "'")
    }
  }
}