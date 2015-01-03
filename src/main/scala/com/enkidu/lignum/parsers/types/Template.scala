package com.enkidu.lignum.parsers.types

sealed trait Template extends Type
sealed trait TemplateParameter extends Template
sealed trait TemplateArgument extends Template

object Template {
  case class Parameter(annotations: Seq[Annotation], name: String) extends TemplateParameter
  case class BoundedParameter(annotations: Seq[Annotation], name: String, upperBound: Type) extends TemplateParameter

  case class Argument(`type`: Type) extends TemplateArgument
  case class Any(annotations: Seq[Annotation]) extends TemplateArgument
  case class AnySubClass(annotations: Seq[Annotation], upperBound: ReferenceType) extends TemplateArgument
  case class AnyBaseClass(annotations: Seq[Annotation], lowerBound: ReferenceType) extends TemplateArgument
}