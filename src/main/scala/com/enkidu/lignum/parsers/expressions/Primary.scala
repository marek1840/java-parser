package com.enkidu.lignum.parsers.expressions

import com.enkidu.lignum.parsers.types._
import com.enkidu.lignum.parsers.statements.MemberDeclaration

case class Select(name: Seq[String]) extends DiscardableExpression

case class MethodReference(qualifier: Expression, types: Seq[TemplateArgument], name: String) extends DiscardableExpression
case class ConstructorReference(`type`: Type, types: Seq[TemplateArgument]) extends DiscardableExpression
case class ArrayConstructorReference(`type`: ArrayType) extends DiscardableExpression
case class FieldAccess(instance: Expression, accesor: String) extends DiscardableExpression
case class MethodInvocation(types: Seq[TemplateArgument], name: String, args: Seq[Expression]) extends DiscardableExpression
case class QualifiedMethodInvocation(instance: Expression, types: Seq[TemplateArgument],
                                     name: String, args: Seq[Expression]) extends DiscardableExpression
case class Extraction(array: Expression, key: Expression) extends DiscardableExpression

case class QualifiedThisReference(name: Seq[String]) extends DiscardableExpression
case class QualifiedParentReference(name: Seq[String]) extends DiscardableExpression
case object ThisReference extends DiscardableExpression
case object ParentReference extends DiscardableExpression

sealed trait Literal extends DiscardableExpression

case class PrimitiveLiteral(value: String) extends Literal
case class ClassLiteral(`type` :Type) extends Literal

sealed trait Instantiation extends DiscardableExpression
sealed trait ObjectInstantiation extends Instantiation with DiscardableExpression
sealed trait ArrayInstantiation extends Instantiation

object Instantiation {
  case class NestedObject(parent: Expression, constructorTypes: Seq[TemplateArgument],
                          `type`: Type, constructorArguments: Seq[Expression]) extends ObjectInstantiation
  case class NestedAnonymousObject(parent: Expression, constructorTypes: Seq[TemplateArgument],
                                   `type`: Type, constructorArguments: Seq[Expression],
                                   body: Seq[MemberDeclaration]) extends ObjectInstantiation

  case class Object(constructorTypes: Seq[TemplateArgument], `type`: Type, constructorArguments: Seq[Expression]) extends ObjectInstantiation
  case class AnonymousObject(constructorTypes: Seq[TemplateArgument], `type`: Type,
                             constructorArguments: Seq[Expression], body: Seq[MemberDeclaration]) extends ObjectInstantiation

  case class Array(`type`: Type, dimensions: Seq[Dimension]) extends ArrayInstantiation
  case class InitializedArray(`type`: Type, initializer: ArrayInitializer) extends ArrayInstantiation
}

sealed trait Dimension

case class AbstractDimension(annotations: Seq[Annotation]) extends Dimension
case class InitializedDimension(annotations: Seq[Annotation], expr: Expression) extends Dimension
