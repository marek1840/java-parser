package com.enkidu.lignum.parsers.ast.expression

import com.enkidu.lignum.parsers.ast.expression.operators.UnaryOperator

case class UnaryOperations(operators: Seq[UnaryOperator], operand: Expression) extends Expression {
  override def dispatch(visitor: Visitor): Unit = {
    operand.dispatch(visitor)
    apply(visitor)
  }
}
