package com.enkidu.lignum.parsers.ast.statement.declarator

import com.enkidu.lignum.parsers.ast.statement.parameter.Parameter

case class MethodDeclarator(name: String, parameters: Seq[Parameter]) extends FunctionDeclarator {
  override def dispatch(visitor: Visitor): Unit = {
    parameters.dispatch(visitor)
    apply(visitor)
  }
}