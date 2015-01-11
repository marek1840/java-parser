package com.enkidu.lignum.parsers.ast.expression.discardable.instantiation

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateArgument

case class SimpleObjectInstantiation(constructorTypes: Seq[TemplateArgument], `type`: Type, constructorArguments: Seq[Expression]) extends ObjectInstantiation {
  override def dispatch(visitor: Visitor): Unit = {
    constructorTypes.dispatch(visitor)
    `type`.dispatch(visitor)
    constructorArguments.dispatch(visitor)
    apply(visitor)
  }
}
