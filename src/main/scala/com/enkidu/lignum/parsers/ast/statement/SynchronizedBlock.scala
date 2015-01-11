package com.enkidu.lignum.parsers.ast.statement

import com.enkidu.lignum.parsers.ast.expression.Expression

case class SynchronizedBlock(lock: Option[Expression], block: Block) extends Statement {
  override def dispatch(visitor: Visitor): Unit = {
    lock.dispatch(visitor)
    block.dispatch(visitor)
    apply(visitor)
  }
}
