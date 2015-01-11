package com.enkidu.lignum.parsers.ast.statement.interruptable

import com.enkidu.lignum.parsers.ast.statement.Block

case class TryCatchFinally(tryBlock: Block, catches: Seq[CatchClause], finallyBlock: Block) extends TryStatement {
  override def dispatch(visitor: Visitor): Unit = {
    tryBlock.dispatch(visitor)
    catches.dispatch(visitor)
    finallyBlock.dispatch(visitor)
    apply(visitor)
  }
}