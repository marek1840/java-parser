package com.enkidu.lignum.parsers.java.v8

import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.{ArrayInitializer, Expression}
import com.enkidu.lignum.parsers.ast.expression.discardable.binary._
import com.enkidu.lignum.parsers.ast.expression.discardable.binary.assignment.{Binding, AugmentedBinding, Assignment}
import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.{AbstractDimension, InitializedDimension, Dimension}
import com.enkidu.lignum.parsers.ast.expression.discardable.instantiation._
import com.enkidu.lignum.parsers.ast.expression.discardable._
import com.enkidu.lignum.parsers.ast.expression.discardable.literals._
import com.enkidu.lignum.parsers.ast.expression.discardable.unary._
import com.enkidu.lignum.parsers.ast.expression.operators._
import com.enkidu.lignum.parsers.ast.expression.types.annotations._
import com.enkidu.lignum.parsers.ast.expression.types.coupled.ChildOfAll
import com.enkidu.lignum.parsers.ast.expression.types.references._
import com.enkidu.lignum.parsers.ast.expression.types.templates._
import com.enkidu.lignum.parsers.ast.statement.declaration.members._
import com.enkidu.lignum.parsers.ast.statement.declaration.types.{InterfaceDeclaration, AnnotationDeclaration, TypeDeclaration}
import com.enkidu.lignum.parsers.ast.statement.flow._
import com.enkidu.lignum.parsers.ast.statement.modifers._
import com.enkidu.lignum.parsers.ast.statement.parameter._
import org.parboiled2.{ CharPredicate, ParserInput, Rule1, Rule2, RuleN }

import shapeless.HNil

abstract class JavaInterfaceParser extends JavaClassParser{
  def interfaceDeclaration: Rule1[TypeDeclaration] = rule {
    interfaceModifiers ~ `interface` ~ identifier ~ optionalTypeParameters ~
      extendedInterfaces ~ `{` ~ interfaceBody ~ `}` ~> InterfaceDeclaration
  }
  def annotationDeclaration: Rule1[TypeDeclaration] = rule {
    interfaceModifiers ~ `@` ~ `interface` ~ identifier ~ `{` ~
      zeroOrMore(annotationMemberDeclaration) ~ `}` ~> AnnotationDeclaration
  }

  private def extendedInterfaces: Rule1[Seq[ClassType]] = rule {
    `extends` ~ (oneOrMore(classType) separatedBy comma) | push(Vector())
  }
  private def interfaceBody: Rule1[Seq[MemberDeclaration]] = rule { zeroOrMore(interfaceMemberDeclaration) }

  def interfaceMemberDeclaration: Rule1[MemberDeclaration] = rule {
    constantDeclaration | interfaceMethodDeclaration | typeDeclaration
  }

  private def constantDeclaration: Rule1[MemberDeclaration] = rule {
    constantModifiers ~ `type` ~ variableDeclarators ~ semicolon ~> ConstantDeclaration
  }

  private def interfaceMethodDeclaration: Rule1[MemberDeclaration] = rule {
    interfaceMethodModifiers ~ methodHeader ~ methodBody ~> InterfaceMethodDeclaration
  }

  protected def annotations: Rule1[Seq[Annotation]] = rule { zeroOrMore(annotation) }

  def annotation: Rule1[Annotation] = rule {
    `@` ~ qualifiedIdentifier ~ {
      `(` ~ (zeroOrMore(elementValuePair) separatedBy `comma`) ~ `)` ~> NormalAnnotation |
        `(` ~ elementValue ~ `)` ~> SingleElementAnnotation |
        MATCH ~> MarkerAnnotation
    }
  }

  private def elementValuePair: Rule1[(String, Expression)] = rule {
    identifier ~ `=` ~ elementValue ~> ((id: String, expr: Expression) => (id, expr))
  }

  private def elementValue: Rule1[Expression] = rule {
    elementValueArrayInitializer | annotation | conditionalExpression
  }

  private def elementValueArrayInitializer: Rule1[Expression] = rule {
    `{` ~ `}` ~ push(ArrayInitializer(Vector())) |
      `{` ~ oneOrMore(elementValue).separatedBy(`comma`) ~ optional(comma) ~ `}` ~> ArrayInitializer
  }

  def annotationMemberDeclaration: Rule1[MemberDeclaration] = rule {
    annotationElementDeclaration | constantDeclaration | typeDeclaration
  }

  private def annotationElementDeclaration: Rule1[MemberDeclaration] = rule {
    annotationElementModifiers ~ `type` ~ identifier ~ `(` ~ `)` ~ optionalDims ~ {
      `default` ~ elementValue ~> { (as: Seq[Annotation], ms: Seq[Modifier], t: Type, n: String, dims: Seq[Dimension], e: Expression) =>
        if (dims.size == 0) AnnotationDefaultElementDeclaration(as, ms, t, n, e)
        else AnnotationDefaultElementDeclaration(as, ms, ArrayType(t, dims), n, e)
      } |
        MATCH ~> { (as: Seq[Annotation], ms: Seq[Modifier], t: Type, n: String, dims: Seq[Dimension]) =>
          if (dims.size == 0) AnnotationElementDeclaration(as, ms, t, n)
          else AnnotationElementDeclaration(as, ms, ArrayType(t, dims), n)
        }
    } ~ semicolon
  }
}
