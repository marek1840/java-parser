package com.enkidu.lignum.parsers.statements

import com.enkidu.lignum.parsers.expressions.Expression

sealed trait MemoryStatement extends Statement

case class DeletionChain(targets:Seq[Expression])extends MemoryStatement
case class Deletion(target:Expression) extends MemoryStatement