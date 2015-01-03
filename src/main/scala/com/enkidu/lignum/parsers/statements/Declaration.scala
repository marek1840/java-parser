package com.enkidu.lignum.parsers.statements

import com.enkidu.lignum.parsers.expressions.Expression
import com.enkidu.lignum.parsers.types.{ Annotation => AnnotationType }
import com.enkidu.lignum.parsers.types.ClassType
import com.enkidu.lignum.parsers.types.Type
import com.enkidu.lignum.parsers.types.TemplateParameter
import com.enkidu.lignum.parsers.expressions.Dimension

trait Declaration extends Statement

sealed trait PackageDeclaration extends Declaration
sealed trait ImportDeclaration extends Declaration
sealed trait MemberDeclaration extends Declaration
sealed trait TypeDeclaration extends MemberDeclaration

object Declaration {
  case class CompilationUnit(`package`: PackageDeclaration, imports: Seq[ImportDeclaration],
                             declared: Seq[TypeDeclaration]) extends Declaration

  case class Package(annotations: Seq[AnnotationType], name: Seq[String]) extends PackageDeclaration
  case object UnnamedPackage extends PackageDeclaration
  case class LazyImport(`package`: Seq[String]) extends ImportDeclaration
  case class SingleImport(qualifier: Seq[String]) extends ImportDeclaration
  case class StaticLazyImport(`package`: Seq[String]) extends ImportDeclaration
  case class StaticImport(qualifier: Seq[String]) extends ImportDeclaration

  case object Empty extends TypeDeclaration
  case class Class(annotations: Seq[AnnotationType], modifiers: Seq[Modifier],
                   name: String, types: Seq[TemplateParameter],
                   baseCLass: Option[ClassType], interfaces: Seq[ClassType],
                   members: Seq[MemberDeclaration]) extends TypeDeclaration
  case class Enum(annotations: Seq[AnnotationType], modifiers: Seq[Modifier],
                  name: String, interfaces: Seq[ClassType], members: Seq[MemberDeclaration]) extends TypeDeclaration

  case class Interface(annotations: Seq[AnnotationType], modifiers: Seq[Modifier], name: String,
                       types: Seq[TemplateParameter], interfaces: Seq[ClassType],
                       body: Seq[MemberDeclaration]) extends TypeDeclaration
  case class Annotation(annotations: Seq[AnnotationType], modifiers: Seq[Modifier],
                        name: String, body: Seq[MemberDeclaration]) extends TypeDeclaration

  case class Field(annotations: Seq[AnnotationType], modifiers: Seq[Modifier],
                   `type`: Type, declarators: Seq[Declarator]) extends MemberDeclaration
  case class Method(annotations: Seq[AnnotationType], modifier: Seq[Modifier],
                    types: Seq[TemplateParameter], resultType: Type,
                    declarator: MethodDeclarator, thrown: Seq[ClassType],
                    body: Statement) extends MemberDeclaration

  case class Constant(annotations: Seq[AnnotationType], modifiers: Seq[Modifier],
                      `type`: Type, declarators: Seq[Declarator]) extends MemberDeclaration
  case class InterfaceMethod(annotations: Seq[AnnotationType], modifier: Seq[Modifier],
                             types: Seq[TemplateParameter], resultType: Type,
                             declarator: MethodDeclarator, thrown: Seq[ClassType],
                             body: Statement) extends MemberDeclaration

  case class AnnotationElement(annotations: Seq[AnnotationType], modifier: Seq[Modifier],
                               `type`: Type, name: String) extends MemberDeclaration
  case class AnnotationDefaultElement(annotations: Seq[AnnotationType], modifier: Seq[Modifier],
                                      `type`: Type, name: String, default: Expression) extends MemberDeclaration

  case class Constructor(annotations: Seq[AnnotationType], modifiers: Seq[Modifier],
                         types: Seq[TemplateParameter], declarator: Declarator.Constructor,
                         thrown: Seq[ClassType], body: Block) extends MemberDeclaration

  case class InstanceInitializer(block: Block) extends MemberDeclaration
  case class StaticInitializer(block: Block) extends MemberDeclaration

  case class EnumConstant(annotations: Seq[AnnotationType], name: String,
                          constructorArguments: Seq[Expression]) extends MemberDeclaration
  case class AnonymousEnumConstant(annotations: Seq[AnnotationType], name: String,
                                   constructorArguments: Seq[Expression], body: Seq[MemberDeclaration]) extends MemberDeclaration

  case class LocalVariable(annotations: Seq[AnnotationType], isFinal: Boolean,
                           `type`: Type, declarators: Seq[Declarator]) extends Declaration
}

sealed trait Declarator
sealed trait MethodDeclarator extends Declarator
object Declarator {
  case class Variable(name: String) extends Declarator
  case class Array(name: String, dimensions: Seq[Dimension]) extends Declarator
  case class InitializedVariable(name: String, initializer: Expression) extends Declarator
  case class InitializedArray(name: String, dimensions: Seq[Dimension], initializer: Expression) extends Declarator

  case class ArrayMethod(name: String, parameters: Seq[FormalParameter], dimensions: Seq[Dimension]) extends MethodDeclarator
  case class Method(name: String, parameters: Seq[FormalParameter]) extends MethodDeclarator

  case class Constructor(name: String, parameters: Seq[FormalParameter]) extends Declarator
}

sealed trait Parameter
sealed trait FormalParameter extends Parameter

object Parameter {
  case class Inferred(name: String) extends Parameter

  case class Formal(annotations: Seq[AnnotationType], isFinal: Boolean,
                    `type`: Type, name: String) extends FormalParameter

  case class InstanceReceiver(annotations: Seq[AnnotationType], `type`: Type) extends FormalParameter
  case class NestedReceiver(annotations: Seq[AnnotationType], `type`: Type, identifier: String) extends FormalParameter

  case class VariableArity(annotations: Seq[AnnotationType], isFinal: Boolean,
                           `type`: Type, name: String) extends FormalParameter
}

sealed trait Modifier

object Modifier {
  case object Abstract extends Modifier
  case object Default extends Modifier
  case object Final extends Modifier
  case object Native extends Modifier
  case object Private extends Modifier
  case object Protected extends Modifier
  case object Public extends Modifier
  case object Static extends Modifier
  case object Strictfp extends Modifier
  case object Synchronized extends Modifier
  case object Transient extends Modifier
  case object Volatile extends Modifier
}