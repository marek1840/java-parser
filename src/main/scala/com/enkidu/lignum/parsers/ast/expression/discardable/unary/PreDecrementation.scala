package com.enkidu.lignum.parsers.ast.expression.discardable.unary

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.discardable.DiscardableExpression

case class PreDecrementation(expression: Expression) extends DiscardableExpression {
  override def dispatch(visitor: Visitor): Unit = {
    expression.dispatch(visitor)
    apply(visitor)
  }
}
