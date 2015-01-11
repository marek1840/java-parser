package com.enkidu.lignum.parsers.ast.expression.discardable

import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateArgument

case class ConstructorReference(`type`: Type, types: Seq[TemplateArgument]) extends DiscardableExpression {
  override def dispatch(visitor: Visitor): Unit = {
    `type`.dispatch(visitor)
    types.dispatch(visitor)
    apply(visitor)
  }
}
