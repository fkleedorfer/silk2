package de.fuberlin.wiwiss.silk

import collection.mutable.Buffer
import de.fuberlin.wiwiss.silk.util.Task
import output.{Output, Link}

/**
* Writes the links to the output.
*/
class OutputTask(links : Buffer[Link], linkType : String, outputs : Traversable[Output]) extends Task[Unit]
{
  taskName = "Writing output"

  override def execute()
  {
    outputs.foreach(_.open)

    for(link <- links;
        output <- outputs)
    {
      output.write(link, linkType)
    }

    outputs.foreach(_.close)
  }
}