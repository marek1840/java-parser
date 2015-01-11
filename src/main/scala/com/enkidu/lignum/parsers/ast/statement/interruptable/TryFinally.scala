package com.enkidu.lignum.parsers.ast.statement.interruptable

import com.enkidu.lignum.parsers.ast.statement.Block

case class TryFinally(tryBlock: Block, finallyBlock: Block) extends TryStatement {
  override def dispatch(visitor: Visitor): Unit = {
    tryBlock.dispatch(visitor)
    finallyBlock.dispatch(visitor)
    apply(visitor)
  }
}