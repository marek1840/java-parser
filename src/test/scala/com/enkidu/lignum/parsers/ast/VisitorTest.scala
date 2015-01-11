package com.enkidu.lignum.parsers.ast

import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.AbstractDimension
import com.enkidu.lignum.parsers.ast.expression.discardable.literals.BooleanLiteral
import com.enkidu.lignum.parsers.ast.expression.types.annotations.MarkerAnnotation
import com.enkidu.lignum.parsers.ast.expression.types.references.ClassType
import com.enkidu.lignum.parsers.ast.expression.types.templates.{AnyTemplate, ParameterTemplate}
import com.enkidu.lignum.parsers.ast.statement.declaration.LocalVariableDeclaration
import com.enkidu.lignum.parsers.ast.statement.declaration.types.EmptyDeclaration
import com.enkidu.lignum.parsers.ast.statement.declarator.VariableDeclarator
import com.enkidu.lignum.parsers.ast.statement.parameter.InferredParameter
import com.enkidu.lignum.parsers.ast.statement.{Block, EmptyStatement}
import org.scalatest.prop.{Checkers, PropertyChecks}
import org.scalatest.{BeforeAndAfterEach, FreeSpec, Matchers}

abstract class VisitorTest extends FreeSpec with PropertyChecks with Matchers with Checkers with BeforeAndAfterEach {
  val dim = AbstractDimension(Vector())
  val declarator = VariableDeclarator("")
  val ann = MarkerAnnotation("")
  val block = Block(Vector())
  val expr = BooleanLiteral("1")
  val stmt = EmptyStatement
  val decl = EmptyDeclaration
  val typ = ClassType(Vector(), None, "A", Vector())
  val arg = AnyTemplate(Vector())
  val argParam = InferredParameter("")
  val templParam = ParameterTemplate(Vector(), "")
  val local = LocalVariableDeclaration(ann, false, typ, declarator)
  val visitor: PartialFunction[Visitable, Unit] = {
    case _ => visited += 1
  }
  protected var visited = 0

  override def beforeEach(): Unit = {
    visited = 0
  }

  protected implicit def toSeq[A](a: A): Seq[A] = Seq(a)

  protected implicit def toOpt[A](a: A): Option[A] = Some(a)

  protected implicit class EasyVisit[A <: Visitable](a: A) {
    def visit() = a.dispatch(visitor)
  }
}
