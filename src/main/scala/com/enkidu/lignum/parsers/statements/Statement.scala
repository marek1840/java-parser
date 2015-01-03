package com.enkidu.lignum.parsers.statements

import com.enkidu.lignum.parsers.expressions.Expression
import com.enkidu.lignum.parsers.types.ClassType
import com.enkidu.lignum.parsers.types.TemplateArgument

trait Statement

case object EmptyStatement extends Statement
case class LabeledStatement(label: String, statement: Statement) extends Statement

case class Block(statements: Seq[Statement]) extends Statement
case class SynchronizedBlock(lock: Option[Expression], block: Block) extends Statement

case class Assertion(condition: Expression, detail: Option[Expression]) extends Statement

sealed trait ConditionalStatement extends Statement

object Conditional {
  case class If(condition: Expression, ifTrue: Statement) extends ConditionalStatement
  case class IfThenElse(condition: Expression, ifTrue: Statement, ifFalse: Statement) extends ConditionalStatement
}

trait Loop extends Statement

object Loop {
  case class For(inits: Seq[Statement], condition: Option[Expression], update: Seq[Statement], body: Statement) extends Loop
  case class Iteration(binding: Declaration.LocalVariable, source: Expression, body: Statement) extends Loop
  case class While(condition: Expression, body: Statement) extends Loop
  case class DoWhile(body: Statement, condition: Expression) extends Loop
}

object ConstructorInvocation {
  case class Alternate(types: Seq[TemplateArgument], argument: Seq[Expression]) extends Statement
  case class Parent(types: Seq[TemplateArgument], argument: Seq[Expression]) extends Statement
  case class IndirectParent(qualifier: Expression, types: Seq[TemplateArgument], argument: Seq[Expression]) extends Statement
  case class Intermidiate(external: Expression, types: Seq[TemplateArgument], argument: Seq[Expression]) extends Statement
}