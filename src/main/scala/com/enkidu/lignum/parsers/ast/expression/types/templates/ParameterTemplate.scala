package com.enkidu.lignum.parsers.ast.expression.types.templates

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class ParameterTemplate(annotations: Seq[Annotation], name: String) extends TemplateParameter {
  override def dispatch(visitor: Visitor): Unit = {
    annotations.dispatch(visitor)
    apply(visitor)
  }
}
