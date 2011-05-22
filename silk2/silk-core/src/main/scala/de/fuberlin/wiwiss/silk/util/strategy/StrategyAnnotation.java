package de.fuberlin.wiwiss.silk.util.strategy;

import java.lang.annotation.ElementType;
import java.lang.annotation.Target;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

@Target(ElementType.TYPE)
@Retention(RetentionPolicy.RUNTIME)
public @interface StrategyAnnotation
{
  String id();
  String label();
  String description() default  "No description";
}
