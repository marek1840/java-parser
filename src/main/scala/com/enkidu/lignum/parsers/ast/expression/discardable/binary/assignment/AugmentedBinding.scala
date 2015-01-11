package com.enkidu.lignum.parsers.ast.expression.discardable.binary.assignment

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.expression.operators.BinaryOperator

case class AugmentedBinding(target: Expression, operator: BinaryOperator, source: Expression) extends Assignment {
  override def dispatch(visitor: Visitor): Unit = {
    target.dispatch(visitor)
    source.dispatch(visitor)
    apply(visitor)
  }
}

