package com.enkidu.lignum.parsers.statements

import com.enkidu.lignum.parsers.expressions.Expression

sealed trait TryStatement extends Statement

case class TryCatch(tryBlock: Block, catches: Seq[CatchClause]) extends TryStatement
case class TryFinally(tryBlock: Block, finallyBlock:Block) extends TryStatement
case class TryCatchFinally(tryBlock: Block, catches: Seq[CatchClause], finallyBlock:Block) extends TryStatement
case class TryWithResources(resources:Seq[Declaration.LocalVariable], tryBlock: Block, catches:Seq[CatchClause], finallyBlock:Option[Block]) extends TryStatement

case class CatchClause(declaration:Declaration.LocalVariable, block:Block)