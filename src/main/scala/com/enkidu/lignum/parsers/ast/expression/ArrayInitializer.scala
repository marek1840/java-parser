package com.enkidu.lignum.parsers.ast.expression

case class ArrayInitializer(values: Seq[Expression]) extends Expression {
  override def dispatch(visitor: Visitor): Unit = {
    values.dispatch(visitor)
    apply(visitor)
  }
}
