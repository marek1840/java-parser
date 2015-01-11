package com.enkidu.lignum.parsers.ast.statement.constructor

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateArgument

case class IndirectParentConstructorInvocation(qualifier: Expression, types: Seq[TemplateArgument], argument: Seq[Expression]) extends ConstructorInvocation {
  override def dispatch(visitor: Visitor): Unit = {
    qualifier.dispatch(visitor)
    types.dispatch(visitor)
    argument.dispatch(visitor)
    apply(visitor)
  }
}

