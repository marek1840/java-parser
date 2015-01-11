package com.enkidu.lignum.parsers.ast.expression

import com.enkidu.lignum.parsers.ast.expression.operators.BinaryOperator

case class BinaryOperations(operators: Seq[BinaryOperator], operands: Seq[Expression]) extends Expression {
  override def dispatch(visitor: Visitor): Unit = {
    operands.dispatch(visitor)
    apply(visitor)
  }
}
