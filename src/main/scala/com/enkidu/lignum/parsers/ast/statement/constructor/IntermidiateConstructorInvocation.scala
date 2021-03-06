package com.enkidu.lignum.parsers.ast.statement.constructor

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateArgument

case class IntermidiateConstructorInvocation(external: Expression, types: Seq[TemplateArgument], argument: Seq[Expression]) extends ConstructorInvocation {
  override def dispatch(visitor: Visitor): Unit = {
    external.dispatch(visitor)
    types.dispatch(visitor)
    argument.dispatch(visitor)
    apply(visitor)
  }
}

