package com.enkidu.lignum.parsers.ast.statement

case class LabeledStatement(label: String, statement: Statement) extends Statement {
  override def dispatch(visitor: Visitor): Unit = {
    statement.dispatch(visitor)
    apply(visitor)
  }
}
