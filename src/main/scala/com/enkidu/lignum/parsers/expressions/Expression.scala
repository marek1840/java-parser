package com.enkidu.lignum.parsers.expressions

import com.enkidu.lignum.parsers.types.TemplateArgument
import com.enkidu.lignum.parsers.types.Type
import com.enkidu.lignum.parsers.types.Template.Parameter
import com.enkidu.lignum.parsers.statements.Statement
import com.enkidu.lignum.parsers.statements

trait Expression

case class Lambda(parameters:Seq[statements.Parameter], body:Statement) extends Expression
case class TernaryConditional(condition: Expression, onTrue: Expression, onFalse: Expression) extends Expression
case class Cast(target: Type, source: Expression) extends Expression

case class BinaryOperationChain(operators: Seq[BinaryOperator], operands: Seq[Expression]) extends Expression
case class UnaryOperationChain(operators: Seq[UnaryOperator], operand: Expression) extends Expression

case class ArrayInitializer(values: Seq[Expression]) extends Expression
