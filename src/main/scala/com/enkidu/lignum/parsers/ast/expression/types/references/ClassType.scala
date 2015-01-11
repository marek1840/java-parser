package com.enkidu.lignum.parsers.ast.expression.types.references

import com.enkidu.lignum.parsers.ast.expression.types.annotations.Annotation
import com.enkidu.lignum.parsers.ast.expression.types.templates.TemplateArgument

case class ClassType(annotations: Seq[Annotation], parent: Option[ClassType],
                     name: String, types: Seq[TemplateArgument]) extends ReferenceType {
  override def dispatch(visitor: Visitor): Unit = {
    annotations.foreach(_.dispatch(visitor))
    parent.foreach(_.dispatch(visitor))
    types.foreach(_.dispatch(visitor))
    apply(visitor)
  }
}
