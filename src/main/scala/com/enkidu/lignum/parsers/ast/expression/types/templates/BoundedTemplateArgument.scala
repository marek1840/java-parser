package com.enkidu.lignum.parsers.ast.expression.types.templates

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.expression.types.references.ReferenceType

trait BoundedTemplateArgument extends TemplateArgument {
  def annotations: Seq[Annotation]

  def bound: ReferenceType

  override def dispatch(visitor: Visitor): Unit = {
    annotations.dispatch(visitor)
    bound.dispatch(visitor)
    apply(visitor)
  }
}
