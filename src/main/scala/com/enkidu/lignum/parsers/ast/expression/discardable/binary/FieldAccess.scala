package com.enkidu.lignum.parsers.ast.expression.discardable.binary

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.discardable.DiscardableExpression

case class FieldAccess(instance: Expression, accesor: String) extends DiscardableExpression {
  override def dispatch(visitor: Visitor): Unit = {
    instance.dispatch(visitor)
    apply(visitor)
  }
}

