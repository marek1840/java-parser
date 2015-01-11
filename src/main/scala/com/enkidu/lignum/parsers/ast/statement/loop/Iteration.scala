package com.enkidu.lignum.parsers.ast.statement.loop

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.statement.Statement
import com.enkidu.lignum.parsers.ast.statement.declaration.LocalVariableDeclaration

case class Iteration(binding: LocalVariableDeclaration, source: Expression, body: Statement) extends Loop {
  override def dispatch(visitor: Visitor): Unit = {
    binding.dispatch(visitor)
    source.dispatch(visitor)
    body.dispatch(visitor)
    apply(visitor)
  }
}