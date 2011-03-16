package de.fuberlin.wiwiss.silk.linkspec

import xml.Node
import de.fuberlin.wiwiss.silk.util.Identifier
import de.fuberlin.wiwiss.silk.config.Prefixes

/**
 * Defines a dataset.
 *
 * @param sourceId The id of the source
 * @param variable Each data item will be bound to this variable.
 * @param restriction Restricts this dataset to specific resources.
 */
case class DatasetSpecification(sourceId : Identifier, variable : String, restriction : Restrictions)
{

  /**
   * Serializes this Dataset Specification as XML.
   *
   * @param If true, this dataset will be serialized as a source dataset. If false it will be serialize as target dataset.
   */
  def toXML(asSource : Boolean) =
  {
    if(asSource)
    {
      <SourceDataset dataSource={sourceId} var={variable}>
        <RestrictTo>{restriction}</RestrictTo>
      </SourceDataset>
    }
    else
    {
      <TargetDataset dataSource={sourceId} var={variable}>
        <RestrictTo>{restriction}</RestrictTo>
      </TargetDataset>
    }
  }
}

object DatasetSpecification
{
  /**
   * Creates a DatasetSpecification from XML.
   */
  def fromXML(node : Node)(implicit prefixes : Prefixes) : DatasetSpecification =
  {
    new DatasetSpecification(
      node \ "@dataSource" text,
      node \ "@var" text,
      Restrictions.fromSparql((node \ "RestrictTo").text.trim)
    )
  }
}
