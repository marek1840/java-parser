package com.enkidu.lignum.parsers.ast.statement.flow

import com.enkidu.lignum.parsers.ast.statement.Statement

trait FlowStatement extends Statement

case object Break extends FlowStatement

case object Continue extends FlowStatement

case object EmptyReturn extends FlowStatement

case class TargetedBreak(target: String) extends FlowStatement

case class TargetedContinue(target: String) extends FlowStatement