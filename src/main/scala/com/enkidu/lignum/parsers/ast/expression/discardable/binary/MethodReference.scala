package com.enkidu.lignum.parsers.ast.expression.discardable.binary

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.discardable.DiscardableExpression
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateArgument

case class MethodReference(qualifier: Expression, types: Seq[TemplateArgument], name: String) extends DiscardableExpression {
  override def dispatch(visitor: Visitor): Unit = {
    qualifier.dispatch(visitor)
    types.dispatch(visitor)
    apply(visitor)
  }
}
