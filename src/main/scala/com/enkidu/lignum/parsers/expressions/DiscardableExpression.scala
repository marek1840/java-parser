package com.enkidu.lignum.parsers.expressions

import com.enkidu.lignum.parsers.statements.Statement
import com.enkidu.lignum.parsers.types.Type
import com.enkidu.lignum.parsers.types.TemplateArgument

trait DiscardableExpression extends Expression with Statement

case class ProcessingChain(primary: Expression, chain: Seq[Expression]) extends DiscardableExpression

case class PreIncrementation(expression: Expression) extends DiscardableExpression
case class PostIncrementation(expression: Expression) extends DiscardableExpression
case class PreDecrementation(expression: Expression) extends DiscardableExpression
case class PostDecrementation(expression: Expression) extends DiscardableExpression

trait Assignment extends DiscardableExpression

case class Binding(target: Expression, source: Expression) extends Assignment
case class AugmentedBinding(target: Expression, operator: BinaryOperator, source: Expression) extends Assignment
case class AssignmentChain(targets: Seq[Expression], source: Expression) extends Assignment
