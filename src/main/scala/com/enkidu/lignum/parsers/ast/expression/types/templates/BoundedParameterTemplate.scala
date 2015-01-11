package com.enkidu.lignum.parsers.ast.expression.types.templates

import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class BoundedParameterTemplate(annotations: Seq[Annotation], name: String, upperBound: Type) extends TemplateParameter {
  override def dispatch(visitor: Visitor): Unit = {
    annotations.dispatch(visitor)
    upperBound.dispatch(visitor)
    apply(visitor)
  }
}
