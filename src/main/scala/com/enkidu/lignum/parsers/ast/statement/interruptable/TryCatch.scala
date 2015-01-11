package com.enkidu.lignum.parsers.ast.statement.interruptable

import com.enkidu.lignum.parsers.ast.statement.Block

case class TryCatch(tryBlock: Block, catches: Seq[CatchClause]) extends TryStatement {
  override def dispatch(visitor: Visitor): Unit = {
    tryBlock.dispatch(visitor)
    catches.dispatch(visitor)
    apply(visitor)
  }
}