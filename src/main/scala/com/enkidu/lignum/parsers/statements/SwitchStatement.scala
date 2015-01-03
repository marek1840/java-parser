package com.enkidu.lignum.parsers.statements

import com.enkidu.lignum.parsers.expressions.Expression

case class SwitchStatement(expression:Expression, cases:Seq[SwitchRule]) extends Statement

sealed trait SwitchRule

object Switch{
  case class EmptyCases(cases:Seq[Expression]) extends SwitchRule
  case class Cases(cases:Seq[Expression], statements: Seq[Statement]) extends SwitchRule

  case object Default extends Expression
}