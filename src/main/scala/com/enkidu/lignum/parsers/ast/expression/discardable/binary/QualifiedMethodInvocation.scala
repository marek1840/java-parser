package com.enkidu.lignum.parsers.ast.expression.discardable.binary

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.discardable.DiscardableExpression
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateArgument

case class QualifiedMethodInvocation(instance: Expression, types: Seq[TemplateArgument],
                                     name: String, args: Seq[Expression]) extends DiscardableExpression {
  override def dispatch(visitor: Visitor): Unit = {
    instance.dispatch(visitor)
    types.dispatch(visitor)
    args.dispatch(visitor)
    apply(visitor)
  }
}
