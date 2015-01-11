package com.enkidu.lignum.parsers.ast.statement.declarator

import com.enkidu.lignum.parsers.ast.statement.parameter.Parameter

case class ConstructorDeclarator(name: String, parameters: Seq[Parameter]) extends Declarator {
  override def dispatch(visitor: Visitor): Unit = {
    parameters.dispatch(visitor)
    apply(visitor)
  }
}