package com.enkidu.lignum.parsers.ast.statement.declarator

import com.enkidu.lignum.parsers.ast.expression.Expression

case class InitializedVariableDeclarator(name: String, initializer: Expression) extends Declarator {
  override def dispatch(visitor: Visitor): Unit = {
    initializer.dispatch(visitor)
    apply(visitor)
  }
}