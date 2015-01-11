package com.enkidu.lignum.parsers.ast.statement.interruptable

import com.enkidu.lignum.parsers.ast.statement.Block
import com.enkidu.lignum.parsers.ast.statement.declaration.LocalVariableDeclaration

case class TryWithResources(resources: Seq[LocalVariableDeclaration], tryBlock: Block, catches: Seq[CatchClause],
                            finallyBlock: Option[Block]) extends TryStatement {
  override def dispatch(visitor: Visitor): Unit = {
    resources.dispatch(visitor)
    tryBlock.dispatch(visitor)
    catches.dispatch(visitor)
    finallyBlock.foreach(_.dispatch(visitor))
    apply(visitor)
  }
}