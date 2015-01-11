package com.enkidu.lignum.parsers.ast.expression.discardable.binary

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.discardable.DiscardableExpression

case class Extraction(array: Expression, key: Expression) extends DiscardableExpression {
  override def dispatch(visitor: Visitor): Unit = {
    array.dispatch(visitor)
    key.dispatch(visitor)
    apply(visitor)
  }
}

