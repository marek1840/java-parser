package com.enkidu.lignum.parsers.ast.statement

case class Block(statements: Seq[Statement]) extends Statement {
  override def dispatch(visitor: Visitor): Unit = {
    statements.dispatch(visitor)
    apply(visitor)
  }
}
