package com.enkidu.lignum.parsers.statements

import com.enkidu.lignum.parsers.expressions.Expression

sealed trait FlowStatement extends Statement

object FlowStatement {
  case object Break extends FlowStatement
  case object Continue extends FlowStatement
  case object EmptyReturn extends FlowStatement

  case class Return(value: Expression) extends FlowStatement
  case class ImplicitReturn(value:Expression) extends FlowStatement
  case class TargetedBreak(target: String) extends FlowStatement
  case class TargetedContinue(target: String) extends FlowStatement

  case class Yield(value: Expression) extends FlowStatement
  case class Throw(exception: Expression) extends FlowStatement
}