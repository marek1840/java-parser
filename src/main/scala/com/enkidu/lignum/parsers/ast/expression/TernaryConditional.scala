package com.enkidu.lignum.parsers.ast.expression

case class TernaryConditional(condition: Expression, onTrue: Expression, onFalse: Expression) extends Expression {
  override def dispatch(visitor: Visitor): Unit = {
    condition.dispatch(visitor)
    onTrue.dispatch(visitor)
    onFalse.dispatch(visitor)
    apply(visitor)
  }
}
