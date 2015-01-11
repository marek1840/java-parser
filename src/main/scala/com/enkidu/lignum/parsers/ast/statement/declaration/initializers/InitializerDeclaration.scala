package com.enkidu.lignum.parsers.ast.statement.declaration.initializers

import com.enkidu.lignum.parsers.ast.statement.Block
import com.enkidu.lignum.parsers.ast.statement.declaration.members.MemberDeclaration

trait InitializerDeclaration extends MemberDeclaration {
  def block: Block

  override def dispatch(visitor: Visitor) = {
    apply(visitor)
    block.dispatch(visitor)
  }
}


