package com.enkidu.lignum.parsers.java.v8

import com.enkidu.lignum.parsers.ast.expression.discardable.dimension._
import com.enkidu.lignum.parsers.ast.expression.types._
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.expression.types.coupled.ChildOfAll
import com.enkidu.lignum.parsers.ast.expression.types.primitives._
import com.enkidu.lignum.parsers.ast.expression.types.references._
import com.enkidu.lignum.parsers.ast.expression.types.templates._
import org.parboiled2.Rule1

abstract class JavaTypeParser extends JavaLiteralParser {
  protected def annotations: Rule1[Seq[Annotation]]

  def `type`: Rule1[Type] = rule {
    (primitiveType | classType) ~ optionalDims ~> {
      (t: Type, ds: Seq[Dimension]) => if (ds.size == 0) t else ArrayType(t, ds)
    }
  }

  protected def primitiveType: Rule1[PrimitiveType] = rule {
    annotations ~ {
      `byte` ~> BytePrimitive |
        `short` ~> ShortPrimitive |
        `float` ~> FloatPrimitive |
        `int` ~> IntegerPrimitive |
        `long` ~> LongPrimitive |
        `char` ~> CharPrimitive |
        `double` ~> DoublePrimitive |
        `boolean` ~> BooleanPrimitive |
        `void` ~> VoidPrimitive
    }
  }

  protected def referenceType: Rule1[ReferenceType] = rule {
    arrayType | classType
  }

  protected def classType: Rule1[ClassType] = rule {
    annotations ~ push(None) ~ identifier ~ optionalTypeArguments ~> ClassType ~ {
      zeroOrMore(dot ~ annotations ~ identifier ~ optionalTypeArguments ~> {
        (p: ClassType, as: Seq[Annotation], id: String, args: Seq[TemplateArgument]) => ClassType(as, Some(p), id, args)
      })
    }
  }

  protected def arrayType: Rule1[ArrayType] = rule {
    (primitiveType | classType) ~ dims ~> ArrayType
  }

  protected def dims: Rule1[Seq[Dimension]] = rule {
    oneOrMore(annotations ~ `[` ~ `]` ~> AbstractDimension)
  }

  protected def optionalDims: Rule1[Seq[Dimension]] = rule {
    zeroOrMore(annotations ~ `[` ~ `]` ~> AbstractDimension)
  }

  protected def typeParameter: Rule1[TemplateParameter] = rule {
    annotations ~ identifier ~ {
      typeBound ~> BoundedParameterTemplate |
        MATCH ~> ParameterTemplate
    }
  }

  private def typeBound: Rule1[Type] = rule {
    `extends` ~ classType ~ zeroOrMore(additionalBound) ~> ((t: ClassType, is: Seq[ClassType]) =>
      if (is.size == 0) t else ChildOfAll(t +: is))
  }

  protected def additionalBound: Rule1[ClassType] = rule {
    `&` ~ classType
  }

  protected def optionalTypeArguments: Rule1[Seq[TemplateArgument]] = rule {
    `<` ~ (zeroOrMore(typeArgument) separatedBy comma) ~ `>` | push(Vector())
  }

  protected def typeArguments: Rule1[Seq[TemplateArgument]] = rule {
    `<` ~ (zeroOrMore(typeArgument) separatedBy comma) ~ `>`
  }

  private def typeArgument: Rule1[TemplateArgument] = rule {
    referenceType ~> ArgumentTemplate |
      annotations ~ {
        `?` ~ `extends` ~ referenceType ~> AnySubClassTemplate |
          `?` ~ `super` ~ referenceType ~> AnyBaseClassTemplate |
          `?` ~> AnyTemplate
      }
  }
}