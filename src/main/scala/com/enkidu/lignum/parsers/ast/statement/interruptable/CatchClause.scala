package com.enkidu.lignum.parsers.ast.statement.interruptable

import com.enkidu.lignum.parsers.ast.Visitable
import com.enkidu.lignum.parsers.ast.statement.Block
import com.enkidu.lignum.parsers.ast.statement.declaration.LocalVariableDeclaration

case class CatchClause(declaration: LocalVariableDeclaration, block: Block) extends Visitable {
  override def dispatch(visitor: Visitor): Unit = {
    declaration.dispatch(visitor)
    block.dispatch(visitor)
    apply(visitor)
  }
}