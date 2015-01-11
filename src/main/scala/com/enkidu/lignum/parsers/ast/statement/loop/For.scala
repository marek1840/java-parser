package com.enkidu.lignum.parsers.ast.statement.loop

import com.enkidu.lignum.parsers.ast.expression.Expression
import com.enkidu.lignum.parsers.ast.statement.Statement

case class For(inits: Seq[Statement], condition: Option[Expression], update: Seq[Statement], body: Statement) extends Loop {
  override def dispatch(visitor: Visitor): Unit = {
    inits.dispatch(visitor)
    condition.dispatch(visitor)
    update.dispatch(visitor)
    body.dispatch(visitor)
    apply(visitor)
  }
}