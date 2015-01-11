package com.enkidu.lignum.parsers.java.v8

import com.enkidu.lignum.parsers.ast.expression._
import com.enkidu.lignum.parsers.ast.expression.types.annotations._
import com.enkidu.lignum.parsers.ast.expression.types.references._
import com.enkidu.lignum.parsers.ast.expression.types.templates._
import com.enkidu.lignum.parsers.ast.statement._
import com.enkidu.lignum.parsers.ast.statement.constructor._
import com.enkidu.lignum.parsers.ast.statement.declaration._
import com.enkidu.lignum.parsers.ast.statement.declaration.initializers._
import com.enkidu.lignum.parsers.ast.statement.declaration.members._
import com.enkidu.lignum.parsers.ast.statement.declaration.types._
import org.parboiled2.Rule1

abstract class JavaClassParser extends JavaStatementParser {
  protected def typeDeclaration: Rule1[TypeDeclaration]

  def annotation: Rule1[Annotation]

  def classDeclaration: Rule1[TypeDeclaration] = rule {
    classModifiers ~ `class` ~ identifier ~ optionalTypeParameters ~
      superClass ~ superInterfaces ~ classBody ~> ClassDeclaration
  }

  def enumDeclaration: Rule1[TypeDeclaration] = rule {
    classModifiers ~ `enum` ~ identifier ~ superInterfaces ~ enumBody ~> EnumDeclaration
  }


  protected def optionalTypeParameters: Rule1[Seq[TemplateParameter]] = rule {
    `<` ~ (oneOrMore(typeParameter) separatedBy comma) ~ `>` | push(Vector())
  }

  private def superClass: Rule1[Option[ClassType]] = rule {
    optional(`extends` ~ classType)
  }

  private def superInterfaces: Rule1[Seq[ClassType]] = rule {
    `implements` ~ (zeroOrMore(classType) separatedBy comma) | push(Vector())
  }

  protected def classBody: Rule1[Seq[MemberDeclaration]] = rule {
    `{` ~ classMembers ~ `}`
  }

  private def classMembers: Rule1[Seq[MemberDeclaration]] = rule {
    zeroOrMore(classMemberDeclaration | instanceInitializer | staticInitializer | constructorDeclaration)
  }

  def classMemberDeclaration: Rule1[MemberDeclaration] = rule {
    fieldDeclaration | methodDeclaration | typeDeclaration
  }

  protected def emptyDeclaration: Rule1[TypeDeclaration] = rule {
    semicolon ~ push(EmptyDeclaration)
  }

  private def fieldDeclaration: Rule1[FieldDeclaration] = rule {
    fieldModifiers ~ `type` ~ variableDeclarators ~ semicolon ~> FieldDeclaration
  }

  protected def variableDeclaration: Rule1[LocalVariableDeclaration] = rule {
    variableModifiers ~ `type` ~ variableDeclarators ~> LocalVariableDeclaration
  }


  private def methodDeclaration: Rule1[MethodDeclaration] = rule {
    methodModifiers ~ methodHeader ~ methodBody ~> MethodDeclaration
  }

  protected def methodBody: Rule1[Statement] = rule {
    block | emptyStatement
  }

  protected def methodHeader = rule {
    optionalTypeParameters ~ `type` ~ methodDeclarator ~ thrown
  }

  private def thrown: Rule1[Seq[ClassType]] = rule {
    `throws` ~ (oneOrMore(classType) separatedBy comma) | push(Vector())
  }


  private def instanceInitializer: Rule1[MemberDeclaration] = rule {
    block ~> InstanceInitializerDeclaration
  }

  private def staticInitializer: Rule1[MemberDeclaration] = rule {
    `static` ~ block ~> StaticInitializerDeclaration
  }

  private def constructorDeclaration: Rule1[ConstructorDeclaration] = rule {
    constuctorModifiers ~ optionalTypeParameters ~ constructorDeclarator ~ thrown ~ constructorBody ~> ConstructorDeclaration
  }

  private def constructorBody: Rule1[Block] = rule {
    `{` ~ optional(explicitConstructorInvocation) ~ zeroOrMore(blockStatement) ~ `}` ~> {
      (s: Option[Statement], ss: Seq[Statement]) => if (s.isDefined) Block(s.get +: ss) else Block(ss)
    }
  }

  private def explicitConstructorInvocation: Rule1[Statement] = rule {
    {
      optionalTypeArguments ~ {
        `this` ~ arguments ~> AlternateConstructorInvocation |
          `super` ~ arguments ~> ParentConstructorInvocation
      } |
        referenceNoMethodAccess ~ dot ~ optionalTypeArguments ~ `super` ~ arguments ~> IndirectParentConstructorInvocation |
        primary ~ dot ~ optionalTypeArguments ~ `super` ~ arguments ~> IntermidiateConstructorInvocation
    } ~ semicolon
  }


  def enumBody: Rule1[Seq[MemberDeclaration]] = rule {
    `{` ~ (zeroOrMore(enumConstant) separatedBy comma) ~ optional(comma) ~ optional(semicolon ~ classMembers) ~ `}` ~> {
      (cs: Seq[MemberDeclaration], ms: Option[Seq[MemberDeclaration]]) => cs ++: ms.getOrElse(Vector())
    }
  }

  private def enumConstant: Rule1[MemberDeclaration] = rule {
    annotations ~ identifier ~ optional(arguments) ~ optional(classBody) ~> {
      (as: Seq[Annotation], n: String, args: Option[Seq[Expression]], b: Option[Seq[MemberDeclaration]]) =>
        if (b.isDefined) AnonymousEnumConstantDeclaration(as, n, args.getOrElse(Vector()), b.get)
        else EnumConstantDeclaration(as, n, args.getOrElse(Vector()))
    }
  }
}
