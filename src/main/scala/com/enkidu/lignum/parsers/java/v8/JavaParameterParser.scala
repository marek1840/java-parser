package com.enkidu.lignum.parsers.java.v8

import com.enkidu.lignum.parsers.ast.expression.discardable.dimension.Dimension
import com.enkidu.lignum.parsers.ast.expression.types.Type
import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.expression.types.references.ArrayType
import com.enkidu.lignum.parsers.ast.statement.parameter._
import org.parboiled2._

abstract class JavaParameterParser extends JavaModifiersParser{
  protected def variableDeclaratorId: Rule2[String, Seq[Dimension]]

  protected def formalParameters: Rule1[Seq[Parameter]] = rule {
    {
      receiverParameter | formalParameter
    } ~ zeroOrMore(comma ~ formalParameter) ~ optional(comma ~ varParameter) ~> {
      (p: Parameter, ps: Seq[Parameter], v: Option[Parameter]) =>
        if (v.isDefined) p +: (ps :+ v.get)
        else p +: ps
    } | varParameter ~> ((p: Parameter) => Vector(p)) |
      MATCH ~ push(Vector())
  }

  private def formalParameter: Rule1[FormalParameter] = rule {
    variableModifiers ~ `type` ~ variableDeclaratorId ~> {
      (as: Seq[Annotation], f: Boolean, t: Type, i: String, ds: Seq[Dimension]) =>
        if (ds.size == 0) FormalParameter(as, f, t, i)
        else FormalParameter(as, f, ArrayType(t, ds), i)
    }
  }

  private def receiverParameter: Rule1[Parameter] = rule {
    annotations ~ `type` ~ {
      identifier ~ dot ~> NestedReceiverParameter |
        MATCH ~> InstanceReceiverParameter
    } ~ `this`
  }

  private def varParameter: Rule1[Parameter] = rule {
    variableModifiers ~ `type` ~ `...` ~ variableDeclaratorId ~> {
      (as: Seq[Annotation], f: Boolean, t: Type, i: String, ds: Seq[Dimension]) =>
        if (ds.size == 0) VariableArityParameter(as, f, t, i)
        else VariableArityParameter(as, f, ArrayType(t, ds), i)
    }
  }
}
