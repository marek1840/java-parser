package com.enkidu.lignum.parsers.ast.expression.types.templates

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation

case class AnyTemplate(annotations: Seq[Annotation]) extends TemplateArgument {
  override def dispatch(visitor: Visitor): Unit = {
    annotations.dispatch(visitor)
    apply(visitor)
  }
}
