package com.enkidu.lignum.parsers.ast.statement.declarator

import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.Dimension
import com.enkidu.lignum.parsers.ast.statement.parameter.Parameter

case class ArrayMethodDeclarator(name: String, parameters: Seq[Parameter], dimensions: Seq[Dimension]) extends FunctionDeclarator {
  override def dispatch(visitor: Visitor): Unit = {
    parameters.dispatch(visitor)
    dimensions.dispatch(visitor)
    apply(visitor)
  }
}