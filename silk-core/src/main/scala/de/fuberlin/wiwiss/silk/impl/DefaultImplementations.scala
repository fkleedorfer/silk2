package de.fuberlin.wiwiss.silk.impl

import classifier._
import datasource._
import transformer._
import aggegrator._
import writer._
import metric._
import comparisonvectorhandler._
import de.fuberlin.wiwiss.silk.datasource.DataSource
import de.fuberlin.wiwiss.silk.linkspec.input.Transformer
import de.fuberlin.wiwiss.silk.output.{LinkWriter, Formatter}
import de.fuberlin.wiwiss.silk.linkspec.condition._
import de.fuberlin.wiwiss.silk.linkspec.FeatureVectorOutput

/**
 * Registers all default implementations.
 */
object DefaultImplementations
{
    def register()
    {
        DataSource.register(classOf[SparqlDataSource])
        DataSource.register(classOf[CacheDataSource])

        Transformer.register(classOf[ReplaceTransformer])
        Transformer.register(classOf[RegexReplaceTransformer])
        Transformer.register(classOf[ConcatTransformer])
        Transformer.register(classOf[RemoveBlanksTransformer])
        Transformer.register(classOf[LowerCaseTransformer])
        Transformer.register(classOf[UpperCaseTransformer])
        Transformer.register(classOf[NumReduceTransformer])
        Transformer.register(classOf[StemmerTransformer])
        Transformer.register(classOf[StripPrefixTransformer])
        Transformer.register(classOf[StripPostfixTransformer])
        Transformer.register(classOf[StripUriPrefixTransformer])
        Transformer.register(classOf[AlphaReduceTransformer])
        Transformer.register(classOf[RemoveSpecialCharsTransformer])
        Transformer.register(classOf[LogarithmTransformer])
        Transformer.register(classOf[ConvertCharsetTransformer])

        Metric.register(classOf[LevenshteinMetric])
        Metric.register(classOf[JaroDistanceMetric])
        Metric.register(classOf[JaroWinklerMetric])
        Metric.register(classOf[QGramsMetric])
        Metric.register(classOf[EqualityMetric])
        Metric.register(classOf[InequalityMetric])
        Metric.register(classOf[NumMetric])
        Metric.register(classOf[DateMetric])
        Metric.register(classOf[GeographicDistanceMetric])

        Aggregator.register(classOf[AverageAggregator])
        Aggregator.register(classOf[MaximumAggregator])
        Aggregator.register(classOf[MinimumAggregator])
        Aggregator.register(classOf[QuadraticMeanAggregator])
        Aggregator.register(classOf[GeometricMeanAggregator])
        Aggregator.register(classOf[SumAggregator])
        
        FeatureVectorOutput.register(classOf[CsvFeatureVectorOutput])

        ClassifierAggregator.register(classOf[RandomClassifierAggregator])
        ClassifierAggregator.register(classOf[FalseClassifierAggregator])
        ClassifierAggregator.register(classOf[WekaClassifierAggregator])

        Extractor.register(classOf[SingleValueExtractor])
        Extractor.register(classOf[IdenticalValuesExtractor])
        Extractor.register(classOf[MinimumValuesExtractor])

        LinkWriter.register(classOf[FileWriter])
        LinkWriter.register(classOf[MemoryWriter])

        Formatter.register(classOf[NTriplesFormatter])
        Formatter.register(classOf[AlignmentFormatter])

        RandomGenerator.register(classOf[BooleanRandomGenerator])
    }
}