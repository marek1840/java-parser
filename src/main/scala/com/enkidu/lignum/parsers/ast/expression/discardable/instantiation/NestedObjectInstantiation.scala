package com.enkidu.lignum.parsers.ast.expression.discardable.instantiation

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateArgument

case class NestedObjectInstantiation(parent: Expression, constructorTypes: Seq[TemplateArgument],
                                     `type`: Type, constructorArguments: Seq[Expression]) extends ObjectInstantiation {
  override def dispatch(visitor: Visitor): Unit = {
    parent.dispatch(visitor)
    constructorTypes.dispatch(visitor)
    `type`.dispatch(visitor)
    constructorArguments.dispatch(visitor)
    apply(visitor)
  }
}