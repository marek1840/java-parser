package com.enkidu.lignum.parsers.ast.expression

import com.enkidu.lignum.parsers.ast.statement.Statement
import com.enkidu.lignum.parsers.ast.statement.parameter.Parameter

case class Lambda(parameters: Seq[Parameter], body: Statement) extends Expression {
  override def dispatch(visitor: Visitor): Unit = {
    parameters.dispatch(visitor)
    body.dispatch(visitor)
    apply(visitor)
  }
}
