package com.enkidu.lignum.parsers.ast.statement.declaration.initializers

import com.enkidu.lignum.parsers.ast.statement.Block

case class StaticInitializerDeclaration(block: Block) extends InitializerDeclaration {
  override def dispatch(visitor: Visitor): Unit = {
    block.dispatch(visitor)
    apply(visitor)
  }
}

