package de.fuberlin.wiwiss.silk.impl.transformer

import de.fuberlin.wiwiss.silk.util.strategy.StrategyAnnotation

@StrategyAnnotation(id = "numReduce", label = "Numeric reduce", description = "Strip all non-numeric characters from a string.")
class NumReduceTransformer() extends RegexReplaceTransformer("[^0-9]+", "")
